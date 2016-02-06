module Upload.Client where

import Upload.Api
import Servant.Client
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.Trans.Either

import Options.Applicative

uploadOpts :: Parser (Maybe (FilePath -> IO ()))
uploadOpts = optional (
    upload <$> option parseBaseUrl' (long "upload-url" <> help "Upload URL")
           <*> option (Key <$> str) (long "upload-key" <> help "Upload access key")
           <*> switch (long "upload")
    )
  where
    parseBaseUrl' :: ReadM BaseUrl
    parseBaseUrl' = either fail pure . parseBaseUrl =<< str

    uploadClient :: BaseUrl -> Maybe Key -> LBS.ByteString -> EitherT ServantError IO ()
    uploadClient = client uploadApi

    upload :: BaseUrl -> Key -> Bool -> FilePath -> IO ()
    upload url key True file = do
        bs <- LBS.readFile file
        ret <- runEitherT $ uploadClient url (Just key) bs
        either (fail . show) return ret
    upload _url _key False _file = return ()
