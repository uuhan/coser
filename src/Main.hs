{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Data.Semigroup            ((<>))
import           Data.Text                 (Text, intercalate)
import           Data.Version              (versionBranch)
import           Data.Word
import           Options.Applicative       hiding (str)
import           Protolude                 hiding (intercalate, (<>))
import           System.Fuse
import           System.Posix.Time         (epochTime)

import           Data.Store
import           Data.Store.QCloud         (appSignMulti)
import           Data.Store.QCloud.FuseOps (qcloudFSOps)

import           Paths_store               (version)
import           Prelude                   hiding (map, putStrLn, show)

type HT = ()

data StoreOptions = StoreOptions
    { ver    :: String
    , appID  :: Word64
    , keyID  :: String
    , key    :: String
    , bucket :: String
    , mnt    :: FilePath
    , debug  :: Bool
    }

options :: Parser StoreOptions
options = StoreOptions
    <$> strOption (
           long "ver"
        <> metavar "<api_verersion>"
        <> value "v2"
        <> showDefault
        <> help "api 版本"
    )
    <*> option auto (
           long "id"
        <> metavar "<app_id>"
        <> help "应用 ID"
    )
    <*> strOption (
           long "kid"
        <> metavar "<key_id>"
        <> help "秘钥 ID"
    )
    <*> strOption (
           long "key"
        <> metavar "<secure_key>"
        <> help "秘钥"
    )
    <*> strOption (
           long "bucket"
        <> metavar "<bucket>"
        <> value "dev"
        <> showDefault
        <> help "仓库"
    )
    <*> strOption (
           long "target"
        <> metavar "<mount_point>"
        <> help "挂载点"
    )
    <*> switch (
           long "debug"
        <> short 'd'
        <> help "调试"
    )

doMain ::
       String -- app id
    -> String -- bucket
    -> String -- version
    -> String -- signature
    -> FilePath
    -> Bool   -- debug
    -> IO ()
doMain app_id bucket ver sig mnt debug = do
    let ops = qcloudFSOps app_id bucket ver sig
    if debug
        then
            fuseRun "store" [mnt, "-d"] ops defaultExceptionHandler
        else
            fuseRun "store" [mnt] ops defaultExceptionHandler

main :: IO ()
main = do
    StoreOptions{..} <- execParser opts
    expire <- toEnum . fromEnum . (+ (24 * 36 * 36)) <$> epochTime -- expired in 1day
    sig <- appSignMulti appID keyID key expire bucket
    doMain (show appID) bucket ver sig mnt debug
  where
    opts :: ParserInfo StoreOptions
    opts = info (options <**> helper)
            $ fullDesc <> progDesc ("store-" <> toS prettyVersion <> " / 挂载对象存储到本地文件系统")

storeOps :: FuseOperations ()
storeOps = defaultFuseOps { fuseRead = readOp }

prettyVersion :: Text
prettyVersion = intercalate "." $ map show $ versionBranch version
