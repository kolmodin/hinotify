module Utils where

import Control.Concurrent.Chan
import Control.Exception

import System.Directory
import System.Environment
import System.Exit

import System.INotify

testName :: IO String
testName = do
    n <- getProgName
    return (n ++ "-playground")

withTempDir :: (String -> IO a) -> IO a
withTempDir f = do
    path <- testName
    bracket
        ( createDirectory path >> return path )
        ( removeDirectoryRecursive )
        ( f )

withWatch :: INotify -> [EventVariety] -> FilePath -> (Event -> IO ()) -> IO a -> IO a
withWatch inot events path action f =
    bracket
        ( addWatch inot events path action )
        removeWatch
        ( const f )

inTestEnviron :: [EventVariety] -> (String -> IO a) -> ([Event] -> IO b) -> IO b
inTestEnviron events action f = do
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
testSuccess = exitWith ExitSuccess

