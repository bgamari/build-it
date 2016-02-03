{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Aeson
import Data.Time.Clock
import Control.Monad.Trans.State
import Control.Monad.IO.Class

data StepSuccess = StepSucceeded
                 | StepFailed { stepFailReason :: String }
    deriving (Generic, ToJSON)

data StepResult = StepResult { stepSucceeded :: StepSuccess
                             , stepRuntime   :: NominalDiffTime
                             , stepArtifacts :: [(ArtifactName, FilePath)]
                             }
    deriving (Generic, ToJSON)

data BuildResult = BuildResult { builderName   :: String
                               , buildSteps    :: [StepResult]
                               }
    deriving (Generic, ToJSON)

newtype ArtifactName = ArtifactName String
    deriving (Generic, ToJSON)

newtype SWriterT w m a = SWriterT (StateT w m a)
    deriving (Functor, Applicative, Monad, MonadIO)

runSWriterT :: (Monad m, Monoid w) => SWriterT w m a -> m (a, w)
runSWriterT (SWriterT action) = runStateT action mempty

tell :: (Monad m, Monoid w) => w -> SWriterT w m ()
tell w = SWriterT $ modify' (`mappend` w)
