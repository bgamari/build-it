{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Build
   ( -- * Basic types
     Verbosity(..)
   , ArtifactName(..)
     -- * Build configuration
   , BuildEnv(..)
   , optBuildEnv
   , defaultBuildEnv
   , simpleBuildEnv
   , getBuildEnv
     -- * Builds
   , Build
   , Build.buildSteps
   , runBuild
   , runAndPackageBuild
     -- * Build steps
   , Step, step
   , StepM
   , logMesg
   , allowFailure
     -- ** External processes
   , cmd
   , run
     -- ** Adding build artifacts
   , copyArtifact
   , addArtifact
   , addArtifactCompressed
   ) where

import System.Process
import System.Exit
import Data.Time.Clock
import System.IO
import Data.Monoid
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad (when)
import Options.Applicative

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL

import System.FilePath
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing, copyFile)
import System.IO.Temp
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.Lzma as Lzma
import GHC.Conc (getNumProcessors)

import Results

data Step a = Step { stepName   :: String
                   , stepAction :: StepM a
                   }

-- | Smart constructor for 'Step'
step :: String -> StepM a -> Step a
step = Step

data Verbosity = Silent | Info | Debug
               deriving (Eq, Ord, Bounded, Enum)

data BuildEnv = BuildEnv { nThreads   :: Int
                         , buildCwd   :: FilePath
                         , scratchDir :: FilePath
                         , verbosity  :: Verbosity
                         , buildName  :: String
                         }

parseVerbosity :: Parser Verbosity
parseVerbosity =
    option (f =<< auto)
           (short 'v' <> long "verbosity" <> metavar "n" <> value Info
           <> help "how much output should we produce to the console?")
  where
    f 0 = pure Silent
    f 1 = pure Info
    f 2 = pure Debug
    f _ = fail "invalid verbosity"

optBuildEnv :: Parser BuildEnv
optBuildEnv =
    BuildEnv <$> option auto (short 'j' <> long "jobs" <> metavar "n" <> value 1
                             <> help "number of concurrent jobs to run")
             <*> option str (short 'C' <> long "directory" <> metavar "dir" <> value "."
                            <> help "the location of the working tree")
             <*> option str (short 't' <> long "temp-dir" <> metavar "dir" <> value "tmp"
                            <> help "where to place temporary files")
             <*> parseVerbosity
             <*> pure "build"

defaultBuildEnv :: BuildEnv
defaultBuildEnv =
    BuildEnv { nThreads   = 1
             , buildCwd   = "."
             , scratchDir = "tmp"
             , verbosity  = Info
             , buildName = "build"
             }

simpleBuildEnv :: IO BuildEnv
simpleBuildEnv = do
    nProcs <- getNumProcessors
    tempDir <- getTemporaryDirectory
    scratchDir <- createTempDirectory tempDir "build"
    pure defaultBuildEnv { nThreads   = nProcs
                         , scratchDir = scratchDir
                         }

-- | An atomic task of a build recipe
newtype StepM a = StepM (EitherT String (StateT StepState (ReaderT BuildEnv IO)) a)
                deriving (Functor, Applicative, Monad, MonadIO)

data StepState = StepS { artifacts :: [(ArtifactName, PackagePath)]
                       , cmdLogNames :: [FilePath]
                       }

data Build = Build [Step ()]

buildSteps :: [Step ()] -> Build
buildSteps = Build

runAndPackageBuild :: BuildEnv -> Build -> IO FilePath
runAndPackageBuild env@(BuildEnv {..}) build = do
    result <- runBuild env build
    let artifact_dir = scratchDir </> buildName
        fname = artifact_dir </> "build.json"
    BSL.writeFile fname $ encode result

    let artifact_paths = "build.json" : [ path
                                        | step <- Results.buildSteps result
                                        , (_, PackagePath path) <- stepArtifacts step
                                        ]

    let tarball_path = artifact_dir <.> "tar.xz"
    BSL.writeFile tarball_path . Lzma.compress
        . Tar.write =<< Tar.pack artifact_dir artifact_paths
    return tarball_path

runBuild :: BuildEnv -> Build -> IO BuildResult
runBuild (env@BuildEnv {..}) (Build steps) = do
    let artifact_dir = scratchDir </> buildName
    createDirectoryIfMissing True artifact_dir
    (_, step_results) <- runSWriterT $ runEitherT $ mapM_ runStep steps
    let res = BuildResult { buildName  = buildName
                          , buildCwd   = buildCwd
                          , buildSteps = step_results
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

-- | Run the given 'StepM' ignoring failure
allowFailure :: StepM () -> StepM ()
allowFailure (StepM action) = do
    res <- StepM $ EitherT (Right <$> runEitherT action)
    case res of
        Right () -> return ()
        Left err -> logMesg Info $ "Allowing failure: "++show err

-- | Run a command sending output to the log
run :: FilePath -> [String] -> StepM ()
run c args = do
    BuildEnv {..} <- getBuildEnv
    log_name:rest <- cmdLogNames <$> StepM (lift get)
    let log_path = scratchDir </> buildName </> log_name
    StepM $ lift $ modify $ \s -> s {cmdLogNames = rest}
    logMesg Info $ "running "++unwords (c:args)++"... "
    code <- liftIO $ withFile log_path WriteMode $ \log_h -> do
        code <- logProcess log_h buildCwd c args
        hPutStrLn log_h $ "$ exit code = "++show code
        return code
    logMesg Info $ "exited with "++show code
    addArtifact (ArtifactName log_name) (PackagePath log_name)
    case code of
        ExitSuccess   -> return ()
        ExitFailure n -> fail $ c++" exited with code "++show n

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

addArtifact :: ArtifactName -> PackagePath -> StepM ()
addArtifact name path =
    StepM $ lift $ modify $ \s -> s { artifacts = (name, path) : artifacts s }

copyArtifact :: ArtifactName -> FilePath -> StepM ()
copyArtifact name file = do
    BuildEnv {..} <- getBuildEnv
    let dest = scratchDir </> buildName </> takeFileName file
    liftIO $ copyFile file dest
    addArtifact name $ PackagePath $ takeFileName file

addArtifactCompressed :: ArtifactName -> FilePath -> StepM ()
addArtifactCompressed name file = do
    BuildEnv {..} <- getBuildEnv
    let dest = scratchDir </> buildName </> takeFileName file <.> "xz"
    liftIO $ BSL.writeFile dest . Lzma.compress =<< BSL.readFile file
    addArtifact name $ PackagePath $ takeFileName file
