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
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

import System.FilePath
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing)
import GHC.Conc (getNumProcessors)

import Types

data Step a = Step { stepName   :: String
                   , stepAction :: StepM a
                   }

step = Step

data BuildEnv = BuildEnv { nThreads   :: Int
                         , buildCwd   :: FilePath
                         , scratchDir :: FilePath
                         , verbose    :: Bool
                         }

defaultBuildEnv :: BuildEnv
defaultBuildEnv =
    BuildEnv { nThreads   = 1
             , buildCwd   = "."
             , scratchDir = "tmp"
             , verbose    = False
             }

simpleBuildEnv :: IO BuildEnv
simpleBuildEnv = do
    nProcs <- getNumProcessors
    tempDir <- getTemporaryDirectory
    pure defaultBuildEnv { nThreads   = nProcs
                         , buildCwd   = "."
                         , scratchDir = tempDir
                         , verbose    = False
                         }

-- | An atomic task of a build recipe
newtype StepM a = StepM (StateT StepState (EitherT String (ReaderT BuildEnv IO)) a)
                deriving (Functor, Applicative, Monad, MonadIO)

data StepState = StepS { artifacts :: [(ArtifactName, FilePath)]
                       , cmdLogNames :: [FilePath]
                       }

data Build = Build [Step ()]

buildSteps :: [Step ()] -> Build
buildSteps = Build

runBuild :: BuildEnv -> Build -> IO ()
runBuild env@(BuildEnv {..}) build = do
    results <- runBuild' env build
    createDirectoryIfMissing True scratchDir
    BS.writeFile "build.json" $ encode results
    return ()

runBuild' :: BuildEnv -> Build -> IO BuildResult
runBuild' env (Build steps) = do
    (_, step_results) <- runSWriterT $ runEitherT $ mapM_ runStep steps
    let res = BuildResult { buildSteps = step_results
                          , builderName = ""
                          }
    return res
  where
    runStep :: Step () -> EitherT () (SWriterT [StepResult] IO) ()
    runStep (Step name (StepM act)) = EitherT $ do
        t0 <- liftIO getCurrentTime
        r <- liftIO $ runReaderT (runEitherT (execStateT act s0)) env
        t1 <- liftIO getCurrentTime
        let success = case r of
                      Left err -> StepFailed err
                      Right _  -> StepSucceeded
        tell [StepResult { stepName      = name
                         , stepSucceeded = success
                         , stepRuntime   = t1 `diffUTCTime` t0
                         , stepArtifacts = []
                         }]
        return $ either (const $ Left ()) (const $ Right ()) r
      where
        s0 = StepS [] [name<>"."<>show i<>".log" | i <- [0..]]

cmd :: FilePath -> [String] -> StepM String
cmd c args =
    liftIO $ readProcess c args ""

getBuildEnv :: StepM BuildEnv
getBuildEnv = StepM $ lift $ lift ask

-- | Run a command sending output to the log
run :: FilePath -> [String] -> StepM ()
run c args = do
    BuildEnv {..} <- getBuildEnv
    log_name:rest <- cmdLogNames <$> StepM get
    let log_path = scratchDir </> log_name <.> ".log"
    StepM $ modify $ \s -> s {cmdLogNames = rest}
    code <- liftIO $ withFile log_path WriteMode $ \log -> logProcess log buildCwd c args
    addArtifact (ArtifactName log_name) log_path
    return ()

logProcess :: Handle -> FilePath -> FilePath -> [String] -> IO ExitCode
logProcess log_h cwd cmd args = do
    queue <- atomically newTQueue
    let worker prefix readH = do
            ls <- map (prefix <>) . BS.lines <$> BS.hGetContents readH
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
                             Right line -> BS.hPutStrLn log_h line >> go
                 in go
    _ <- waitBoth a b
    atomically $ putTMVar done ()
    wait w
    waitForProcess ph

addArtifact :: ArtifactName -> FilePath -> StepM ()
addArtifact name file = StepM $ modify $ \s -> s { artifacts = (name, file) : artifacts s }

addArtifactCompressed :: ArtifactName -> FilePath -> StepM ()
addArtifactCompressed name file = do
    run "xz" [file]
    addArtifact name (file++".xz")
