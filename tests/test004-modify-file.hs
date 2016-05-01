{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import Control.Monad

import System.Directory
import System.IO

import System.INotify as INotify

import Utils

file :: String
file = "hello"

write :: String -> IO ()
write path =
    writeFile (path ++ '/':file) ""

modify :: String -> IO ()
modify path =
    bracket
        (openFile (path ++ '/':file) AppendMode)
        hClose
        (\h -> hPutStr h "yarr!")

remove :: String -> IO ()
remove path =
    removeFile (path ++ '/':file)

action :: String -> IO ()
action path = do
    write path
    modify path
    remove path

main :: IO ()
main =
    inTestEnviron [AllEvents] action $ \ events -> do
        when (expected ~= events)
            testSuccess
        putStrLn $ explainFailure expected events
        testFailure

expected :: [Event]
expected =
    [ Created   False "hello"
    , Opened    False (Just "hello")
    , Modified  False (Just "hello")
    , Closed    False (Just "hello") True
    , Opened    False (Just "hello")
    , Modified  False (Just "hello")
    , Closed    False (Just "hello") True
    , Deleted   False "hello"
    ]
