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


file, file2 :: ByteString
file = "hello"
file2 = file `BS.append` "2"

write :: ByteString -> IO ()
write path =
  bracket (createFile (path `BS.append` "/" `BS.append` file) stdFileMode)
          closeFd
          (\fd -> void $ fdWrite fd " ")

move :: ByteString -> IO ()
move path =
    rename (path `BS.append` "/" `BS.append` file)
           (path `BS.append` "/" `BS.append` file2)

remove :: ByteString -> IO ()
remove path =
    removeLink (path `BS.append` "/" `BS.append` file2)

action :: ByteString -> IO ()
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
