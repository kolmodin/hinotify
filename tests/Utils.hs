{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Control.Concurrent.Chan
import Control.Exception

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import Data.String

import System.Directory ( removeDirectoryRecursive )
import System.Environment
import System.Exit

import System.INotify

import System.Posix.ByteString.FilePath
import System.Posix.Directory.ByteString
import System.Posix.Files.ByteString

testName :: IO RawFilePath
testName = do
    n <- getProgName
    return (fromString n `B.append` "-playground")

withTempDir :: (RawFilePath -> IO a) -> IO a
withTempDir f = do
    path <- testName
    bracket
        ( createDirectory path ownerModes >> return path )
        ( removeDirectoryRecursive . fromString . BC8.unpack )
        f

withWatch :: INotify -> [EventVariety] -> RawFilePath -> (Event -> IO ()) -> IO a -> IO a
withWatch inot events path action f =
    bracket
        ( addWatch inot events path action )
        removeWatch
        ( const f )

inTestEnviron :: [EventVariety] -> (FilePath -> IO a) -> ([Event] -> IO b) -> IO b
inTestEnviron events action f =
    withTempDir $ \testPath -> do
        inot <- initINotify
        chan <- newChan
        withWatch inot events testPath (writeChan chan) $ do
            _ <- action (fromString . BC8.unpack $ testPath)
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

