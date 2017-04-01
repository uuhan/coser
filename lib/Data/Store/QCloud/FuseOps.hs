{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Data.Store.QCloud.FuseOps
    (qcloudFSOps)
  where

import           Data.Aeson
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.String
import           Foreign.C.Error
import           Network.Curl
import           System.Fuse
import           System.Posix.Files
import           System.Posix.Types

import           Data.Store.QCloud.Internal (FileInfo (..), Resp (..))

type HT = ()

qcloudFSOps :: String -> String -> String -> String -> FuseOperations HT
qcloudFSOps app_id bucket version sig =
    defaultFuseOps { fuseGetFileStat        = getFileStatOp sig op_url
                   , fuseOpen               = openOp
                   , fuseRead               = readOp sig dw_url -- ref @ openOp
                   , fuseRemoveLink         = rmOp sig op_url -- ref @ openOp
                   , fuseOpenDirectory      = openDirOp
                   , fuseReadDirectory      = readDirOp
                   , fuseGetFileSystemStats = getFileSystemStatsOp
                   }
  where
    op_url = "http://sh.file.myqcloud.com/files/" ++ version ++ "/" ++ app_id ++ "/" ++ bucket ++ "/fuse"
    dw_url = "http://" ++ bucket ++ "-" ++ app_id ++ ".cossh.myqcloud.com/fuse"

qcloudString :: B.ByteString
qcloudString = B.pack "Hello World, HFuse!\n"

testPath :: FilePath
testPath = "/test.txt"

dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

fileStat :: FuseContext -> FileStat
fileStat ctx = FileStat { statEntryType = RegularFile
                        , statFileMode = foldr1 unionFileModes
                                           [ ownerReadMode
                                           , groupReadMode
                                           , otherReadMode
                                           ]
                        , statLinkCount = 1
                        , statFileOwner = fuseCtxUserID ctx
                        , statFileGroup = fuseCtxGroupID ctx
                        , statSpecialDeviceID = 0
                        , statFileSize = fromIntegral $ B.length qcloudString
                        , statBlocks = 1
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }

getFileStatOp :: String -> String -> FilePath -> IO (Either Errno FileStat)
getFileStatOp sig url "/" = do
    ctx <- getFuseContext
    return $ Right $ dirStat ctx

getFileStatOp sig url path | path == testPath = do
    ctx <- getFuseContext
    (_, s) <- curlGetString (url ++ testPath ++ "?op=stat") [CurlVerbose True, CurlHttpHeaders ["Authorization: " ++ sig]]
    case decode (BL.pack s) :: Maybe Resp of
        Just Resp{..} -> do
            return $ Right $ fileStat ctx
        Nothing -> do
            putStrLn s
            return $ Left eNOENT

getFileStatOp _ _ _ =
    return $ Left eNOENT

openDirOp :: (Monad m, Data.String.IsString a, Eq a) => a -> m Errno
openDirOp "/" = return eOK
openDirOp _   = return eNOENT

readDirOp :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
readDirOp "/" = do
    ctx <- getFuseContext
    return $ Right [(".",          dirStat  ctx)
                   ,("..",         dirStat  ctx)
                   ,(testName,     fileStat ctx)
                   ]
    where
      (_:testName) = testPath
readDirOp _ = return (Left (eNOENT))

openOp :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
openOp path mode _ -- flags
    | path == testPath = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | otherwise = return (Left eNOENT)


readOp :: String -> String -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
readOp sig url path _ byteCount offset
    | path == testPath = do
        (_, s) <- curlGetString (url ++ testPath) [CurlVerbose True, CurlHttpHeaders ["Authorization: " ++ sig]]
        return $ Right $ B.pack s
    | otherwise = do
        print path
        return $ Left eNOENT

rmOp :: String -> String -> FilePath -> IO Errno
rmOp sig url path = do
    undefined

getFileSystemStatsOp :: String -> IO (Either Errno FileSystemStats)
getFileSystemStatsOp _ = -- str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }
