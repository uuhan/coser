{-# LANGUAGE TypeFamilies #-}
module Data.Store
  where

import qualified Data.ByteString.Char8 as B
import           Data.String
import           Foreign.C.Error
import           System.Fuse
import           System.Posix.Files
import           System.Posix.Types

import           Network.Curl          as C

type HT = ()

helloFSOps :: FuseOperations HT
helloFSOps = defaultFuseOps { fuseGetFileStat        = getFileStatOp
                            , fuseOpen               = openOp
                            , fuseRead               = readOp -- ref @ openOp
                            , fuseOpenDirectory      = openDirOp
                            , fuseReadDirectory      = readDirOp
                            , fuseGetFileSystemStats = getFileSystemStatsOp
                            }

helloString :: B.ByteString
helloString = B.pack "Hello World, HFuse!\n"

helloPath :: FilePath
helloPath = "/local"

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
                        , statFileSize = fromIntegral $ B.length helloString
                        , statBlocks = 1
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }

getFileStatOp :: FilePath -> IO (Either Errno FileStat)
getFileStatOp "/" = do
    ctx <- getFuseContext
    return $ Right $ dirStat ctx

getFileStatOp path | path == helloPath = do
    ctx <- getFuseContext
    return $ Right $ fileStat ctx

getFileStatOp path | path == testPath = do
    ctx <- getFuseContext
    return $ Right $ fileStat ctx

getFileStatOp _ =
    return $ Left eNOENT

openDirOp :: (Monad m, Data.String.IsString a, Eq a) => a -> m Errno
openDirOp "/" = return eOK
openDirOp _   = return eNOENT

readDirOp :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
readDirOp "/" = do
    ctx <- getFuseContext
    return $ Right [(".",          dirStat  ctx)
                   ,("..",         dirStat  ctx)
                   ,(helloName,    fileStat ctx)
                   ,(testName,    fileStat ctx)
                   ]
    where
      (_:helloName) = helloPath
      (_:testName) = testPath
readDirOp _ = return (Left (eNOENT))

openOp :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
openOp path mode _ -- flags
    | path == helloPath = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | path == testPath = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | otherwise         = return (Left eNOENT)


readOp :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
readOp path _ byteCount offset
    | path == testPath = do
        print ("http://localhost:3000" ++ testPath)
        (_, s) <- curlGetString ("http://localhost:3000" ++ testPath) []
        return $ Right $ B.pack s
    | path == helloPath =
        return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) helloString
    | otherwise         = do
        print path
        return $ Left eNOENT

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
