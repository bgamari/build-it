{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Upload.Api where

import qualified Data.ByteString.Lazy as LBS
import Servant

newtype Key = Key String
            deriving (FromText, ToText)

type UploadApi =
    "upload" :> Header "key" Key
             :> ReqBody '[OctetStream] LBS.ByteString
             :> Post '[JSON] ()

uploadApi :: Proxy UploadApi
uploadApi = Proxy
