{-# LANGUAGE OverloadedStrings #-}

module Trigger where

import Blockchain.Data.DataDefs
import Blockchain.Data.BlockDB
import Blockchain.Database.MerklePatricia
import Control.Monad
import Data.ByteString.Char8 (unpack)
import Database.Esqueleto

import SQLFunctions
import SQLMonad

notifyName = "quarry"
extraKey = "bestBlockNumber"

setupTriggers :: ConnT ()
setupTriggers conn = asSimpleTransaction $ mapM_ executeSimple [
  clearTrigger txTrigger txTable,
  clearTrigger bestTrigger bestTable,
  createTriggerFunction txFunc notifyName txData,
  createTriggerFunction bestFunc notifyName bestData,
  createTrigger txTrigger txEvent txTable txEach txCond txFunc,
  createTrigger bestTrigger bestEvent bestTable bestEach bestCond bestFunc,
  listenTrigger notifyName
  ]
  where
    txTrigger = "quarry_tx"         ; bestTrigger = "quarry_best"
    txTable = "raw_transaction"     ; bestTable = "extra"
    txFunc = "quarry_tx"            ; bestFunc = "quarry_best"
    txData = "'TX (' || new || ')'" ; bestData = "'BB ' || new.value"
    txEvent = "after insert"        ; bestEvent = "after insert"
    txEach = "row"                  ; bestEach = "row"
    txCond = ""                     ; bestCond = "new.the_key = '" ++ extraKey ++ "'"

data NotifyData =
  NewBestBlock (Entity Block) |
  NewTransaction Transaction

data NotifyPayload =
  BB (SHAPtr, Integer) |
  TX RawTransaction
  deriving (Read)

waitNotifyData :: ConnT NotifyData
waitNotifyData = do
  Notification _ notifChannel notifData <- waitNotification
  guard $ notifChannel == notifyName
  case read $ unpack notifData of
    BB (stateRoot, _) -> NewBestBlock <$> blockFromStateRoot stateRoot
    TX rawTX -> return $ NewTransaction $ rawTX2TX rawTX

blockFromStateRoot :: SHAPtr -> ConnT (Entity Block)
blockFromStateRoot stateRoot = asPersistTransaction $ do
  blocks <-
    select $
    from $ \(b `InnerJoin` bdr) -> do
      on (b ^. BlockId ==. bdr ^. BlockDataRefBlockId &&.
          bdr ^. BlockDataRefStateRoot ==. val stateRoot)
      return b
  return $ head blocks

getBestBlock :: ConnT (Entity Block)
getBestBlock = do
  Extra { value = stateRoot } <- asPersistTransaction $
    select $ from $ \ex -> do
      ex ^. ExtraKey ==. val extraKey
      return ex
  blockFromStateRoot stateRoot
