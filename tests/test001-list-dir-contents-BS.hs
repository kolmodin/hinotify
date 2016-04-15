module Main where

import Control.Monad

import System.INotify.ByteString

import qualified System.Posix.Directory.Traversals as PT

import Utils.ByteString


main :: IO ()
main =
  inTestEnviron [Open, Close] PT.getDirectoryContents $ \ events -> do
        when (expected ~= events)
            testSuccess
        putStrLn $ explainFailure expected events
        testFailure


expected :: [Event]
expected =
    [ Opened True Nothing
    , Closed True Nothing False
    ]
