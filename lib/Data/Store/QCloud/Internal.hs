module Data.Store.QCloud.Internal
    (FileInfo(..), Resp(..))
  where

import           Data.Aeson
import           Data.Word

data FileInfo = FileInfo
    { access_url :: String
    -- , authority  :: String
    -- , biz_attr   :: String
    , ctime      :: Word64
    -- , custom_headers :: [CustomHeaders]
    }

data Resp = Resp
    { code    :: Int
    , message :: String
    , datA    :: FileInfo
    }

instance FromJSON FileInfo where
    parseJSON = withObject "fileinfo" $ \o ->
        FileInfo <$> o .: "access_url"
                 <*> o .: "ctime"

instance FromJSON Resp where
    parseJSON = withObject "response" $ \o ->
        Resp <$> o .: "code"
             <*> o .: "message"
             <*> o .: "data"
