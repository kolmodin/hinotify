module Main where

import Control.Concurrent

import System.INotify as INotify
import System.Timeout

import Utils

file :: String
file = "hello"

write :: String -> IO ()
write path = do
    writeFile (path ++ '/':file) ""

main :: IO ()
main = maybe testFailure (const testSuccess) =<< timeout 1000000 doTest

doTest :: IO ()
doTest =
    withTempDir $ \testPath -> do
        inot <- initINotify
        mvar1 <- newEmptyMVar
        mvar2 <- newEmptyMVar
        _ <- addWatch inot [AllEvents] testPath $ \_event -> do
            putMVar mvar1 ()
            takeMVar mvar2 -- hangs here
        write testPath
        takeMVar mvar1
        killINotify inot -- should complete and kill all threads
