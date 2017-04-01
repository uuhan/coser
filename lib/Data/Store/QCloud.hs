{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Store.QCloud
    ( appSignMulti
    , appSignOnce
    )
  where

import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types

foreign import ccall unsafe "appSignMulti"
    appSignMulti_ ::
            CULong  -- 项目app_id
         -> CString -- 秘钥ID
         -> CString -- 秘钥
         -> CULong  -- 过期时间/epoch
         -> CString -- 文件所在bucket
         -> IO CString

foreign import ccall unsafe "appSignOnce"
    appSignOnce_ ::
            CULong  -- 项目app_id
         -> CString -- 秘钥ID
         -> CString -- 秘钥
         -> CString -- 文件路径
         -> CString -- 文件所在bucket
         -> IO CString

appSignMulti :: Word64 -> String -> String -> Word64 -> String -> IO String
appSignMulti app_id key_id key epoch bucket = do
    let app_id_ = CULong app_id
    let epoch_ = CULong epoch
    sign <- withCString key_id $ \key_id_ -> do
        withCString key $ \key_ -> do
            withCString bucket $ \bucket_ -> do
                appSignMulti_ app_id_ key_id_ key_ epoch_ bucket_
    peekCString sign

appSignOnce :: Word64 -> String -> String -> String -> String -> IO String
appSignOnce app_id key_id key path bucket = do
    let app_id_ = CULong app_id
    sign <- withCString key_id $ \key_id_ -> do
        withCString key $ \key_ -> do
            withCString path $ \path_ -> do
                withCString bucket $ \bucket_ -> do
                    appSignOnce_ app_id_ key_id_ key_ path_ bucket_
    peekCString sign
