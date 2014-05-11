module Main where

import Control.Monad

import System.Directory

import System.INotify as INotify

import Utils

main :: IO ()
main =
    inTestEnviron [Open, Close] getDirectoryContents $ \ events -> do
        when (expected ~= events)
            testSuccess
        putStrLn $ explainFailure expected events
        testFailure

expected :: [Event]
expected =
    [ Opened True Nothing
    , Closed True Nothing False
    ]
