{-# LANGUAGE OverloadedStrings #-}
module Trigger (setupTrigger) where

import Blockchain.Data.DataDefs
import Blockchain.Database.MerklePatricia
import Control.Monad
import Data.ByteString
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification

-- Exported

waitGetBestBlock :: Connection -> IO Block
waitGetBestBlock conn = do
  Notification _ notifChannel notifData <- getNotification conn
  guard $ notifChannel == triggerName
  let (stateRoot, _) = read $ unpack notifData
  queryBestBlock conn stateRoot  

setupTrigger :: Connection -> IO ()
setupTrigger conn = withTransaction conn $ sequence_ [
  clearTrigger, createTriggerFunction, createTrigger, listenTrigger
  ]

-- Internal

queryBestBlock :: Connection -> SHAPtr -> IO Block
queryBestBlock conn stateRoot = -- What do I do here?

clearTrigger :: Connection -> IO Int64
clearTrigger conn = execute conn "drop trigger if exists ? on ?" (triggerName, bestBlockDB)

createTriggerFunction :: Connection -> IO Int64
createTriggerFunction conn =
  execute conn
  ("create or replace function ?() returns trigger language plpgsql as " ++
   "$$begin" ++
   " perform pg_notify(?, new.value)" ++
   " return null;" ++
   "end$$")
  (triggerName, bestBlockNotify)

createTrigger :: Connection -> IO Int64
createTrigger conn =
  execute conn
  "create trigger ? after insert on ? for each row when new.? = ? execute procedure ?()"
  (triggerName, bestBlockDB, bestBlockKeyCol, bestBlockKey, triggerName)

listenTrigger :: Connection -> IO Int64
listenTrigger conn = execute conn "listen ?" (Only triggerName)

-- Global constant names

triggerName = "newBestNotify"
bestBlockDB = "extra"
bestBlockKey = "bestBlockNumber"
bestBlockKeyCol = "the_key"
bestBlockNotify = "bestBlockNotify"
