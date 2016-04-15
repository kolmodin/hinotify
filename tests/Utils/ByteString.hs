{-# LANGUAGE OverloadedStrings #-}

module Utils.ByteString where


import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 (fromString)

import Control.Concurrent.Chan
import Control.Exception

import GHC.IO.Exception ( IOErrorType(InappropriateType) )

import System.Environment
import System.Exit

import System.INotify.ByteString

import System.IO.Error
  ( ioeSetErrorString
  , ioeSetLocation
  , mkIOError
  , modifyIOError )

import System.Posix.Directory.ByteString
import System.Posix.Directory.Traversals
import System.Posix.FilePath
import System.Posix.Files.ByteString



testName :: IO ByteString
testName = do
    n <- getProgName
    return (fromString n `BS.append` "-playground")

withTempDir :: (ByteString -> IO a) -> IO a
withTempDir f = do
    path' <- testName
    bracket
        ( createDirectory path' accessModes >> return path' )
        removeDirectoryRecursive
        f

withWatch :: INotify -> [EventVariety] -> ByteString -> (Event -> IO ()) -> IO a -> IO a
withWatch inot events path action f =
    bracket
        ( addWatch inot events path action )
        removeWatch
        ( const f )

inTestEnviron :: [EventVariety] -> (ByteString -> IO a) -> ([Event] -> IO b) -> IO b
inTestEnviron events action f =
    withTempDir $ \testPath -> do
        inot <- initINotify
        chan <- newChan
        withWatch inot events testPath (writeChan chan) $ do
            _ <- action testPath
            events' <- getChanContents chan
            f events'

(~=) :: Eq a => [a] -> [a] -> Bool
[] ~= _ = True
(x:xs) ~= (y:ys) = x == y && xs ~= ys
_ ~= _ = False

asMany :: [a] -> [a] -> [a]
asMany xs ys = take (length xs) ys

explainFailure :: Show a => [a] -> [a] -> String
explainFailure expected reality = unlines $
    [ "Expected:" ] ++
    [ "> " ++ show x | x <- expected ] ++
    [ "But got:" ] ++
    [ "< " ++ show x | x <- asMany expected reality ]

testFailure, testSuccess :: IO a
testFailure = exitFailure 
testSuccess = exitSuccess


removeDirectoryRecursive :: ByteString -> IO ()
removeDirectoryRecursive path =
  (`ioeSetLocation` "removeDirectoryRecursive") `modifyIOError` do
    stat <- getSymbolicLinkStatus path
    if System.Posix.Files.ByteString.isDirectory stat
       then removeContentsRecursive path
       else ioError . (`ioeSetErrorString` "not a directory") $
            mkIOError InappropriateType "" Nothing (Just $ show path)


removeContentsRecursive :: ByteString -> IO ()
removeContentsRecursive path =
  (`ioeSetLocation` "removeContentsRecursive") `modifyIOError` do
    cont <- (filter (\(_, y) -> y /= "." && y /= ".."))
            `fmap` getDirectoryContents path
    mapM_ removePathRecursive $ fmap (\(_, y) -> path </> y) cont
    removeDirectory path


removePathRecursive :: ByteString -> IO ()
removePathRecursive path =
  (`ioeSetLocation` "removePathRecursive") `modifyIOError` do
  stat <- getSymbolicLinkStatus path
  if System.Posix.Files.ByteString.isDirectory stat
     then removeContentsRecursive path
     else removeLink path

