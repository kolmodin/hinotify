{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.INotify.ByteString

import System.Posix.Files.ByteString
import System.Posix.IO.ByteString

import Utils.ByteString

write :: ByteString -> IO ()
write path =
    bracket (createFile (path `BS.append` "/hello") stdFileMode)
            closeFd
            (\fd -> void $ fdWrite fd " ")
    
main :: IO ()
main =
    inTestEnviron [AllEvents] write $ \ events -> do
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
    ]
