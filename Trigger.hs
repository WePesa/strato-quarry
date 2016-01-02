{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Trigger (waitGetBestBlock, setupTrigger) where

import Blockchain.Data.DataDefs
import Blockchain.Database.MerklePatricia
import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString.Char8 as BC
import Data.Int
import Database.Esqueleto hiding (Connection)
import Database.Persist.Postgresql (withPostgresqlConn, runSqlConn)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification

-- Exported

waitGetBestBlock :: Connection -> IO (Entity Block)
waitGetBestBlock conn = do
  Notification _ notifChannel notifData <- getNotification conn
  guard $ notifChannel == BC.pack triggerName
  let (stateRoot, _ :: Integer) = read $ BC.unpack notifData
  queryBestBlock conn stateRoot  

setupTrigger :: Connection -> IO ()
setupTrigger conn = withTransaction conn $ sequence_ $ map ($ conn) [
  clearTrigger, createTriggerFunction, createTrigger, listenTrigger
  ]

-- Internal

queryBestBlock :: Connection -> SHAPtr -> IO (Entity Block)
queryBestBlock conn stateRoot =
  let connStr = postgreSQLConnectionString defaultConnectInfo
  in runNoLoggingT $ withPostgresqlConn connStr $ runSqlConn $ do
    blocks <-
      select $
      from $ \(b `InnerJoin` bdr) -> do
        on (b ^. BlockId ==. bdr ^. BlockDataRefBlockId &&.
            bdr ^. BlockDataRefStateRoot ==. val stateRoot)
        return b
    return $ head blocks

clearTrigger :: Connection -> IO Int64
clearTrigger conn = execute conn "drop trigger if exists ? on ?" (triggerName, bestBlockDB)

createTriggerFunction :: Connection -> IO Int64
createTriggerFunction conn =
  execute conn
  ("create or replace function ?() returns trigger language plpgsql as \
   \$$begin\n\
   \ perform pg_notify('?', new.value);\n\
   \ return null;\n\
   \end$$")
  (triggerName, bestBlockNotify)

createTrigger :: Connection -> IO Int64
createTrigger conn =
  execute conn
  "create trigger ? after insert on ? for each row when new.? = ? execute procedure ?()"
  (triggerName, bestBlockDB, bestBlockKeyCol, bestBlockKey, triggerName)

listenTrigger :: Connection -> IO Int64
listenTrigger conn = execute conn "listen ?" (Only bestBlockNotify)

-- Global constant names

triggerName = "new_best_notify" :: String
bestBlockDB = "extra" :: String
bestBlockKey = "bestBlockNumber" :: String
bestBlockKeyCol = "the_key" :: String
bestBlockNotify = "best_block_notify" :: String
