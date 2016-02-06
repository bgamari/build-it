{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import qualified Data.ByteString.Lazy as LBS
import Servant
import Servant.Server
import Network.Wai.Handler.WarpTLS as WarpTLS
import qualified Network.Wai.Handler.Warp as Warp
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class

import Upload.Api

knownKeys :: [Key]
knownKeys = [ Key "ben-server" ]

server :: [(Key, BuilderName)] -> Server UploadApi
server builders = upload
  where
    upload :: Maybe Key -> LBS.ByteString -> EitherT ServantErr IO ()
    upload (Just key) _data
      | Just name <- key `lookup` builders = do
        let fname = key
        lift $ LBS.writeFile name _data
        return ()
    upload _ _ = left err403


type BuilderName = String
readKnownBuilders :: IO [(Key, BuilderName)]
readKnownBuilders = foldMap f . lines <$> readFile "known-builders"
  where
    f line = case words line of [] -> []
                                [key,name] -> [(Key key, name)]

main :: IO ()
main = do
    builders <- readKnownBuilders
    let app = serve uploadApi $ server builders
    WarpTLS.runTLS WarpTLS.defaultTlsSettings settings app
  where
    settings = Warp.setPort 9090
               $ Warp.defaultSettings
