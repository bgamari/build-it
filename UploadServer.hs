{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import qualified Data.ByteString.Lazy as LBS
import Servant
import Network.Wai.Handler.WarpTLS as WarpTLS
import qualified Network.Wai.Handler.Warp as Warp
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class

import Upload.Api

server :: Server UploadApi
server = upload
  where
    upload :: Maybe Key -> LBS.ByteString -> EitherT ServantErr IO ()
    upload (Just (Key key)) _data = do
        let fname = key
        lift $ LBS.writeFile fname _data
        return ()

app = serve uploadApi server

main :: IO ()
main = WarpTLS.runTLS WarpTLS.defaultTlsSettings settings app
  where
    settings = Warp.setPort 9090
               $ Warp.defaultSettings
