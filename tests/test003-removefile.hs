{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad

import System.Directory

import System.INotify as INotify

import Utils

file :: String
file = "hello"

write :: String -> IO ()
write path = do
    writeFile (path ++ '/':file) ""

remove :: String -> IO ()
remove path = do
    removeFile (path ++ '/':file)

action :: String -> IO ()
action path = do
    write path
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
    , Deleted   False "hello"
    ]
