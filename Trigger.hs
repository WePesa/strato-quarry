{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Trigger (waitGetBestBlock, setupTrigger) where

import Blockchain.Data.DataDefs
import Blockchain.Database.MerklePatricia
import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Char8 (pack)
import Data.Int
import Database.Esqueleto hiding (Connection)
import Database.Persist.Postgresql (withPostgresqlConn, runSqlConn)
import Database.PostgreSQL.Simple 
import Database.PostgreSQL.Simple.Notification
import Database.PostgreSQL.Simple.Types

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
clearTrigger conn =
  doQuery conn $ "drop trigger if exists " ++ triggerName ++ " on " ++ bestBlockDB

createTriggerFunction :: Connection -> IO Int64
createTriggerFunction conn =
  doQuery conn $
  "create or replace function " ++ triggerName ++ "() \
  \returns trigger language plpgsql as \
  \$$begin \
  \ perform pg_notify('" ++ bestBlockNotify ++ "', new.value); \
  \ return null; \
  \end$$"

createTrigger :: Connection -> IO Int64
createTrigger conn =
  doQuery conn $
  "create trigger " ++ triggerName ++ " after insert on " ++ bestBlockDB ++
  " for each row when (new." ++ bestBlockKeyCol ++ " = " ++ bestBlockKey ++ ") " ++
  " execute procedure " ++ triggerName ++ "()"

listenTrigger :: Connection -> IO Int64
listenTrigger conn = doQuery conn $ "listen " ++ bestBlockNotify

doQuery :: Connection -> String -> IO Int64
doQuery conn s = execute conn (Query . pack $ s) ()

-- Global constant names

triggerName = "new_best_notify" :: String
bestBlockDB = "extra" :: String
bestBlockKey = "bestBlockNumber" :: String
bestBlockKeyCol = "the_key" :: String
bestBlockNotify = "best_block_notify" :: String
