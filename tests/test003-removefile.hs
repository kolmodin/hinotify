{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad

import Data.String

import System.Directory

import System.INotify as INotify

import Utils

file :: IsString s => s
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
    [ Created   False file
    , Opened    False (Just file)
    , Modified  False (Just file)
    , Closed    False (Just file) True
    , Deleted   False file
    ]
