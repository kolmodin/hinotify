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


file :: ByteString
file = "hello"

write :: ByteString -> IO ()
write path =
  bracket (createFile (path `BS.append` "/" `BS.append` file) stdFileMode)
          closeFd
          (\fd -> void $ fdWrite fd " ")

modify :: ByteString -> IO ()
modify path =
  bracket (openFd (path `BS.append` "/" `BS.append` file) WriteOnly
                  Nothing defaultFileFlags)
          closeFd
          (\fd -> void $ fdWrite fd "yarr!")

remove :: ByteString -> IO ()
remove path =
    removeLink (path `BS.append` "/" `BS.append` file)

action :: ByteString -> IO ()
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
    [ Created   False file
    , Opened    False (Just file)
    , Modified  False (Just file)
    , Closed    False (Just file) True
    , Opened    False (Just file)
    , Modified  False (Just file)
    , Closed    False (Just file) True
    , Deleted   False file
    ]
