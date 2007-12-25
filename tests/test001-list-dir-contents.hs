module Main where

import Control.Monad

import System.Directory

import System.INotify as INotify

import Utils

main =
    inTestEnviron [Open, Close] getDirectoryContents $ \ events -> do
        when (expected ~= events)
            testSuccess
        explainFailure expected events

expected =
    [ Opened True Nothing
    , Closed True Nothing False
    ]
