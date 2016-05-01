{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad

import System.Directory

import System.INotify as INotify

import Utils

file, file2 :: String
file = "hello"
file2 = "hello2"

write :: String -> IO ()
write path = do
    writeFile (path ++ '/':file) ""

move :: String -> IO ()
move path = do
    renameFile (path ++ '/':file) (path ++ '/':file2)

remove :: String -> IO ()
remove path = do
    removeFile (path ++ '/':file2)

action :: String -> IO ()
action path = do
    write path
    move path
    remove path

main :: IO ()
main =
    inTestEnviron [AllEvents] action $ \ events -> do
        let cookie = head [ c | MovedOut _ _ c <- events ]
        when (expected cookie ~= events)
            testSuccess
        putStrLn $ explainFailure (expected cookie) events
        testFailure

expected :: Cookie -> [Event]
expected cookie =
    [ Created   False "hello"
    , Opened    False (Just "hello")
    , Modified  False (Just "hello")
    , Closed    False (Just "hello") True
    , MovedOut  False "hello"  cookie
    , MovedIn   False "hello2" cookie
    , Deleted   False "hello2"
    ]
