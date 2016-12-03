{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as BC8
import Control.Concurrent
import Control.Exception

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
    withTempDir $ \testPath ->
    bracket
        initINotify
        killINotify   -- should complete and kill all threads
        $ \inot -> do
            mvar1 <- newEmptyMVar
            mvar2 <- newEmptyMVar
            _ <- addWatch inot [AllEvents] testPath $ \_event -> do
                putMVar mvar1 ()
                takeMVar mvar2 -- hangs here
            write (BC8.unpack testPath)
            takeMVar mvar1
