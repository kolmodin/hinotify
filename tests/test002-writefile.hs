module Main where

import Control.Monad

import System.INotify as INotify

import Utils

write path = do
    writeFile (path ++ "/hello") ""
    -- actually writing any contents gives me two Modified
    
main =
    inTestEnviron [Open, Access, Modify, Close] write $ \ events -> do
        when (expected ~= events)
            testSuccess
        explainFailure expected events

expected =
    [ Opened    False (Just "hello")
    , Modified  False (Just "hello")
    , Closed    False (Just "hello") True
    ]
