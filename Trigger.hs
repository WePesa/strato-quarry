{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Trigger where

import Blockchain.Data.DataDefs
import Blockchain.Data.BlockDB
import Blockchain.Data.Transaction
import Blockchain.Database.MerklePatricia
import Control.Monad
import Control.Monad.Logger
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe
import Data.String
import Database.Esqueleto
import Database.Persist.Class
import Database.PostgreSQL.Simple.Notification

import SQLFunctions
import SQLMonad

import Debug.Trace

notifyName = "quarry"
extraKey = "bestBlockNumber"

setupTriggers :: ConnT ()
setupTriggers = asSimpleTransaction [
  clearTrigger txTrigger txTable,
  clearTrigger bestTrigger bestTable,
  createTriggerFunction txFunc notifyName txData,
  createTriggerFunction bestFunc notifyName bestData,
  createTrigger txTrigger txTable txFunc,
  createTrigger bestTrigger bestTable bestFunc,
  listenTrigger notifyName
  ]
  where
    txTrigger = "quarry_tx"    ; bestTrigger = "quarry_best"
    txTable = "raw_transaction"; bestTable = "extra"
    txFunc = "quarry_tx"       ; bestFunc = "quarry_best"
    txData = "'TX'"            ; bestData = "'BB'"

data NotifyData =
  NewBestBlock (Entity Block) |
  NewTransactions [Transaction]

data NotifyPayload = BB | TX deriving (Read, Show)

waitNotifyData :: ConnT NotifyData
waitNotifyData = do
  Notification {notificationData = notifData} <- waitNotification
  --NewTransactions <$> getNewTransactions
  case notifData of --read $ unpack notifData of
    "BB" -> do
      trace "Got best block" $ return ()
      NewBestBlock <$> getBestBlock
    "TX" -> do
      trace "Got new transaction(s)" $ return ()
      NewTransactions <$> getNewTransactions
    x -> error $ "notifData was: " ++ show x ++ "."

getBestBlock :: ConnT (Entity Block)
getBestBlock = do
  Just (Entity {entityVal = Extra {extraValue = eV}}) <-
    asPersistTransaction $ getBy (TheKey extraKey)
  let (stateRoot, _ :: Integer) = read eV
  blockFromStateRoot stateRoot

blockFromStateRoot :: SHAPtr -> ConnT (Entity Block)
blockFromStateRoot stateRoot = asPersistTransaction $ do
  blocks <-
    select $
    from $ \(b `InnerJoin` bdr) -> do
      on (b ^. BlockId ==. bdr ^. BlockDataRefBlockId &&.
          bdr ^. BlockDataRefStateRoot ==. val stateRoot)
      return b
  return $ head blocks

getNewTransactions :: ConnT [Transaction]
getNewTransactions = asPersistTransaction $ do
  rawtxEs <-
    select $
    from $ \rawtx -> do
      where_ (rawtx ^. RawTransactionBlockNumber ==. val (-1))
      return rawtx
  return $ map (rawTX2TX . entityVal) rawtxEs
