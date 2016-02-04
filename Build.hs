{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Build where

import System.Process
import Data.Time.Clock
import System.IO
import System.Exit (ExitCode)
import Data.Monoid
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Applicative ((<|>))
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad (when, forM)

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL

import System.FilePath
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing, copyFile)
import System.IO.Temp
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.Lzma as Lzma
import GHC.Conc (getNumProcessors)

import Types

data Step a = Step { stepName   :: String
                   , stepAction :: StepM a
                   }

step = Step

data Verbosity = Silent | Info | Debug
               deriving (Eq, Ord, Bounded, Enum)

data BuildEnv = BuildEnv { nThreads   :: Int
                         , buildCwd   :: FilePath
                         , scratchDir :: FilePath
                         , verbosity  :: Verbosity
                         }

defaultBuildEnv :: BuildEnv
defaultBuildEnv =
    BuildEnv { nThreads   = 1
             , buildCwd   = "."
             , scratchDir = "tmp"
             , verbosity  = Info
             }

simpleBuildEnv :: IO BuildEnv
simpleBuildEnv = do
    nProcs <- getNumProcessors
    tempDir <- getTemporaryDirectory
    scratchDir <- createTempDirectory tempDir "build"
    pure defaultBuildEnv { nThreads   = nProcs
                         , buildCwd   = "."
                         , scratchDir = scratchDir
                         , verbosity  = Info
                         }

-- | An atomic task of a build recipe
newtype StepM a = StepM (EitherT String (StateT StepState (ReaderT BuildEnv IO)) a)
                deriving (Functor, Applicative, Monad, MonadIO)

data StepState = StepS { artifacts :: [(ArtifactName, FilePath)]
                       , cmdLogNames :: [FilePath]
                       }

data Build = Build [Step ()]

buildSteps :: [Step ()] -> Build
buildSteps = Build

scratchFile :: FilePath -> StepM FilePath
scratchFile name = (name </>) . scratchDir <$> getBuildEnv

runBuild :: BuildEnv -> Build -> IO FilePath
runBuild env@(BuildEnv {..}) build = do
    putStrLn $ "Using scratch directory "++scratchDir
    createDirectoryIfMissing True scratchDir
    result <- runBuild' env build
    let fname = scratchDir </> "build.json"
    BSL.writeFile fname $ encode result
    let artifact_paths = "build.json" : [ path
                                        | step <- Types.buildSteps result
                                        , (_, path) <- stepArtifacts step
                                        ]
    let tarball_path = scratchDir </> "build.tar.xz"
    BSL.writeFile tarball_path . Lzma.compress
        . Tar.write =<< Tar.pack scratchDir artifact_paths
    putStrLn $ "Build tarballl in "++tarball_path
    return tarball_path

runBuild' :: BuildEnv -> Build -> IO BuildResult
runBuild' (env@BuildEnv {..}) (Build steps) = do
    (_, step_results) <- runSWriterT $ runEitherT $ mapM_ runStep steps
    let res = BuildResult { buildSteps = step_results
                          , builderName = ""
                          }
    return res
  where
    runStep :: Step () -> EitherT () (SWriterT [StepResult] IO) ()
    runStep (Step name (StepM act)) = EitherT $ do
        t0 <- liftIO getCurrentTime
        (r, state') <- liftIO $ runReaderT (runStateT (runEitherT act) s0) env
        t1 <- liftIO getCurrentTime
        let success = case r of
                      Left err -> StepFailed err
                      Right _  -> StepSucceeded
        tell [StepResult { stepName      = name
                         , stepSucceeded = success
                         , stepRuntime   = t1 `diffUTCTime` t0
                         , stepArtifacts = artifacts state'
                         }]
        return $ either (const $ Left ()) (const $ Right ()) r
      where
        s0 = StepS [] [name<>"."<>show i<>".log" | i <- [0..]]

cmd :: FilePath -> [String] -> StepM String
cmd c args =
    liftIO $ readProcess c args ""

getBuildEnv :: StepM BuildEnv
getBuildEnv = StepM $ lift $ lift ask

logMesg :: Verbosity -> String -> StepM ()
logMesg v msg = do
    BuildEnv {..} <- getBuildEnv
    when (v >= verbosity) $ liftIO $ putStrLn msg

-- | Run a command sending output to the log
run :: FilePath -> [String] -> StepM ()
run c args = do
    BuildEnv {..} <- getBuildEnv
    log_name:rest <- cmdLogNames <$> StepM (lift get)
    let log_path = scratchDir </> log_name
    StepM $ lift $ modify $ \s -> s {cmdLogNames = rest}
    logMesg Info $ "running "++unwords (c:args)++"... "
    code <- liftIO $ withFile log_path WriteMode $ \log_h -> do
        code <- logProcess log_h buildCwd c args
        hPutStrLn log_h $ "$ exit code = "++show code
        return code
    logMesg Info $ "exited with "++show code
    addArtifact (ArtifactName log_name) log_path
    return ()

logProcess :: Handle -> FilePath -> FilePath -> [String] -> IO ExitCode
logProcess log_h cwd cmd args = do
    hPutStrLn log_h $ "= "++unwords (cmd:args)
    queue <- atomically newTQueue
    let worker prefix readH = do
            ls <- map (prefix <>) . BSL.lines <$> BSL.hGetContents readH
            mapM_ (atomically . writeTQueue queue) ls

    (_, Just h_stdout, Just h_stderr, ph) <-
        createProcess (proc cmd args) { std_out = CreatePipe, std_err = CreatePipe, cwd = Just cwd }

    a <- async $ worker "< " h_stdout
    b <- async $ worker "! " h_stderr

    done <- atomically newEmptyTMVar
    w <- async $ let go = do
                         res <- atomically $ (Right <$> readTQueue queue)
                                         <|> (Left <$> readTMVar done)
                         case res of
                             Left ()    -> return ()
                             Right line -> BSL.hPutStrLn log_h line >> go
                 in go
    _ <- waitBoth a b
    atomically $ putTMVar done ()
    wait w
    waitForProcess ph

addArtifact :: ArtifactName -> FilePath -> StepM ()
addArtifact name file =
    StepM $ lift $ modify $ \s -> s { artifacts = (name, file) : artifacts s }

copyArtifact :: ArtifactName -> FilePath -> StepM ()
copyArtifact name file = do
    BuildEnv {..} <- getBuildEnv
    let dest = scratchDir </> file
    liftIO $ copyFile file dest
    addArtifact name dest

addArtifactCompressed :: ArtifactName -> FilePath -> StepM ()
addArtifactCompressed name file = do
    BuildEnv {..} <- getBuildEnv
    let dest = scratchDir </> file <.> "xz"
    liftIO $ BSL.writeFile dest . Lzma.compress =<< BSL.readFile file
    addArtifact name dest
