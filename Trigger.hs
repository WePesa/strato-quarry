module Trigger where

import Control.Monad.IO.Class
import Data.ByteString.Char8 (unpack)
import Database.PostgreSQL.Simple.Notification

import SQLFunctions
import SQLMonad

import Debug.Trace

data NotifyChannel = QuarryNewTX | QuarryBestBlock deriving (Read, Show)

setupTriggers :: ConnT ()
setupTriggers = do
  liftIO $ putStrLn "Creating triggers and listeners"
  asSimpleTransaction [
    clearTrigger txName txTable,
    clearTrigger bestName bestTable,
    createTriggerFunction txName txName,
    createTriggerFunction bestName bestName,
    createTrigger txName txEvent txTable txName,
    createTrigger bestName bestEvent bestTable bestName,
    listenTrigger txName,
    listenTrigger bestName
    ]
  where
    txName = show QuarryNewTX  ; bestName = show QuarryBestBlock
    txTable = "raw_transaction"; bestTable = "extra"
    txEvent = "insert"         ; bestEvent = "update"

waitNotifyData :: ConnT NotifyChannel
waitNotifyData = do
  liftIO $ putStrLn "Waiting for the next notification"
  Notification {notificationChannel = c} <- waitNotification
  return $ read $ unpack c
