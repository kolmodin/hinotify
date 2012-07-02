module MoveSpec where

import Data.Maybe

import Control.Monad

import System.Directory
import System.IO

import System.INotify as INotify

import Utils

file = "hello"
file2 = file ++ "2"

write path = do
    writeFile (path ++ '/':file) ""

move path = do
    renameFile (path ++ '/':file) (path ++ '/':file2)

remove path = do
    removeFile (path ++ '/':file2)

action path = do
    write path
    move path
    remove path
    
main =
    inTestEnviron [AllEvents] action $ \ events -> do
        let cookie = head [ c | MovedOut _ _ c <- events ]
        when (expected cookie ~= events)
            testSuccess
        explainFailure (expected cookie) events

expected cookie =
    [ Created   False file
    , Opened    False (Just file)
    , Modified  False (Just file)
    , Closed    False (Just file) True
    , MovedOut  False file  cookie
    , MovedIn   False file2 cookie
    , Deleted   False file2
    ]

