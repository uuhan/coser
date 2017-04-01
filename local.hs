{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Internal     (Request (..))

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson
import           System.Directory
import           System.IO

app :: Application
app Request{..} = do
    case (requestMethod, pathInfo) of
        ("GET", []) -> return list
        _           -> return notFound

list = do
    l <- liftIO $ getDirectoryContents "./3rd" >>= pure . filter (\x -> x /= "." && x /= "..")
    responseLBS
        status200
        [(hContentType, "application/json")]
        (encode l)

notFound = responseLBS
    status200
    [(hContentType, "text/plain")]
    "Not Found"

main :: IO ()
main = run 3000 app
