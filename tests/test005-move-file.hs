{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad

import Data.String

import System.Directory

import System.INotify as INotify

import Utils

file, file2 :: IsString s => s
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
    [ Created   False file
    , Opened    False (Just file)
    , Modified  False (Just file)
    , Closed    False (Just file) True
    , MovedOut  False file  cookie
    , MovedIn   False file2 cookie
    , Deleted   False file2
    ]
