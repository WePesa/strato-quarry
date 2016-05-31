{-# LANGUAGE OverloadedStrings #-}

module SimpleSQL where

import Control.Monad.Logger
import Data.ByteString.Char8 (unpack)
import Database.PostgreSQL.Simple.Notification
import SQLMonad

clearTrigger :: String -> String -> String
clearTrigger name table = "drop trigger if exists " ++ name ++ " on " ++ table

createTriggerFunction :: String -> String -> String
createTriggerFunction func notify =
  "create or replace function " ++ func ++ "() " ++
  "returns trigger language plpgsql as " ++
  "$$begin \n" ++
  " perform pg_notify('" ++ notify ++ "', null);\n" ++
  " return null;\n" ++
  "end$$"

createTrigger :: String -> String -> String -> String -> String -> Maybe String -> String
createTrigger name event table func scope condM =
  "create trigger " ++ name ++
  " after " ++ event ++ " on " ++ table ++
  " for each " ++ scope ++
  maybe "" (\s -> " when (" ++ s ++ ")") condM ++
  " execute procedure " ++ func ++ "()"  

listenTrigger :: String -> String
listenTrigger name = "listen " ++ show name

data NotifyChannel = QuarryNewTX | QuarryBestBlock deriving (Read, Show)

setupTriggers :: [String]
setupTriggers = [
  clearTrigger txName txTable,
  clearTrigger bestName bestTable,
  createTriggerFunction txName txName,
  createTriggerFunction bestName bestName,
  createTrigger txName txEvent txTable txName txScope txCond,
  createTrigger bestName bestEvent bestTable bestName bestScope bestCond,
  listenTrigger txName,
  listenTrigger bestName
  ]
  where
    txName = show QuarryNewTX  ; bestName = show QuarryBestBlock
    txTable = "raw_transaction"; bestTable = "extra"
    txEvent = "insert"         ; bestEvent = "update"
    txScope = "statement"      ; bestScope = "row"
    txCond = Nothing           ; bestCond = Just $ "new.the_key = 'bestBlock'"

waitNotifyData :: ConnT NotifyChannel
waitNotifyData = do
  logInfoN "Waiting for the next notification"
  Notification {notificationChannel = c} <- waitNotification
  return $ read $ unpack c
