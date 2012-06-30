module Utils where

import Control.Concurrent.Chan
import Control.Exception

import System.Directory
import System.Environment
import System.Exit

import System.INotify

testName = do
    n <- getProgName
    return (n ++ "-playground")

withTempDir f = do
    path <- testName
    bracket
        ( createDirectory path >> return path )
        ( removeDirectoryRecursive )
        ( f )

withEventWatch inot events path f =
    bracket
        ( watch inot events path action )
        removeWatch
        ( f )

withWatch inot events path action f =
    bracket
        ( addWatch inot events path action )
        removeWatch
        ( const f )

inTestEnviron events action f = do
    withTempDir $ \testPath -> do
        inot <- initINotify
        chan <- newChan
        withWatch inot events testPath (writeChan chan) $ do
            action testPath
            events <- getChanContents chan
            f events

inTestEnvironEvent events action f = do
    withTempDir $ \testPath -> do
        inot <- initINotify
        chan <- newChan
        withEventWatch inot events testPath $ \(wd,chan) -> do
            action testPath
            events <- getChanContents chan
            f events


(~=) :: Eq a => [a] -> [a] -> Bool
[] ~= _ = True
(x:xs) ~= (y:ys) = x == y && xs ~= ys
_ ~= _ = False

asMany :: [a] -> [a] -> [a]
asMany xs ys = take (length xs) ys

explainFailure expected reality = do
    putStrLn "Expected:"
    mapM_ (\x -> putStr "> " >> print x) expected
    putStrLn "But got:"
    mapM_ (\x -> putStr "< " >> print x) (asMany expected reality)
    testFailure

testFailure = exitFailure 

testSuccess = exitWith ExitSuccess
