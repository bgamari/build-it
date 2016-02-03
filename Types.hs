{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

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

newtype SWriterT w m a = SWriterT {getSWriterT :: StateT w m a}
    deriving (Functor, MonadIO)

instance Monad m => Applicative (SWriterT w m) where
    pure = SWriterT . pure
    SWriterT a <*> SWriterT b = SWriterT $ a <*> b

instance Monad m => Monad (SWriterT w m) where
    return = pure
    SWriterT a >>= f = SWriterT $ a >>= getSWriterT . f

runSWriterT :: (Monad m, Monoid w) => SWriterT w m a -> m (a, w)
runSWriterT (SWriterT action) = runStateT action mempty

tell :: (Monad m, Monoid w) => w -> SWriterT w m ()
tell w = SWriterT $ modify' (`mappend` w)
