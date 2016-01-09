{-# LANGUAGE OverloadedStrings #-}

module BlockConstruction where

import Blockchain.Data.BlockDB
import Blockchain.Data.RLP
import Blockchain.Data.Transaction
import Blockchain.Database.MerklePatricia hiding (Key)
import Blockchain.EthConf
import Blockchain.SHA
import Blockchain.Verifier

import Control.Monad.IO.Class

import Data.Time.Clock

import Database.Persist.Sql

import PersistSQL
import Debug

data DBBlock = DBBlock {
  dbBlock :: Block,
  dbBlockIds :: Maybe BlockIds
  }

updateBlock :: (MonadIO m) => DBBlock -> SqlPersistT m DBBlock
updateBlock oldDBBlock = do
  liftIO $ putStrLn "New transactions: update previous block"
  txs <- getNewTransactions
  let oldBlock = dbBlock oldDBBlock
      oldBIdsM = dbBlockIds oldDBBlock
      oldTXs = blockReceiptTransactions oldBlock
      newTXs = oldTXs ++ txs
      b = oldBlock { blockReceiptTransactions = newTXs }
  bids <- makeBlockIds b
  maybe
    (debugPrint "\t(New block)\n---\n")
    (
      \oldBIds@(bId, bdId) -> do
        deleteBlockQ oldBIds
        debugPrint $
          "\tReplacing block " ++
          "(bId: " ++ show bId ++ ", bdId: " ++ show bdId ++ ")\n" ++
          "---\n"
    )
    oldBIdsM
  return DBBlock {
    dbBlock = b,
    dbBlockIds = Just bids
    }

makeNewBlock :: (MonadIO m) => SqlPersistT m DBBlock
makeNewBlock = do
  liftIO $ putStrLn "New best block: making a new block"
  newBest <- getBestBlock
  txs <- getGreenTXs newBest
  b <- constructBlock newBest txs
  bidsM <-
    if not . null $ blockReceiptTransactions b
    then do
      r <- Just <$> makeBlockIds b
      debugPrint "\t(New block)\n---\n"
      return r
    else do
      debugPrint "Empty block; not committing\n"
      return Nothing
  return DBBlock {
    dbBlock = b,
    dbBlockIds = bidsM
    }

makeBlockIds :: (MonadIO m) => Block -> SqlPersistT m BlockIds
makeBlockIds b = do
  bids@(bId, bdId) <- putBlock b
  debugPrint $ "--- \n" ++
    "\tInserted block (bId: " ++ show bId ++ ", bdId: " ++ show bdId ++ ")\n" ++
    "\tBlock hash: " ++ show (blockHash b) ++ "\n" ++
    "\tIncluding transactions: \n" ++
    (concatMap (\t -> "\t\tTX Hash: " ++ show (transactionHash t) ++ "\n") $
     blockReceiptTransactions b)
  return bids

constructBlock :: (MonadIO m) => Entity Block -> [Transaction] -> SqlPersistT m Block
constructBlock parentE txs = do
  let parent = entityVal parentE
      parentData = blockBlockData parent
  uncles <- getSiblings parent
  time <- liftIO getCurrentTime
  return $ Block {
    blockBlockUncles = uncles,
    blockReceiptTransactions = txs,
    blockBlockData = BlockData {
      blockDataParentHash = blockHash parent,
      blockDataUnclesHash = hash . rlpSerialize . RLPArray $ map rlpEncode uncles,
      blockDataCoinbase = fromIntegral $ coinbaseAddress $ quarryConfig ethConf,
      blockDataStateRoot = SHAPtr "",
      blockDataTransactionsRoot = emptyTriePtr,
      blockDataReceiptsRoot = emptyTriePtr,
      blockDataLogBloom =
        "0000000000000000000000000000000000000000000000000000000000000000",
      blockDataDifficulty =
        nextDifficulty
        (blockDataNumber parentData)
        (blockDataDifficulty parentData)
        (blockDataTimestamp parentData)
        time,
      blockDataNumber = blockDataNumber (blockBlockData parent) + 1,
      blockDataGasLimit =
        let g = blockDataGasLimit $ blockBlockData parent
            (q,d) = g `quotRem` 1024
        in g + q - (if d == 0 then 1 else 0),
      blockDataGasUsed = 0,
      blockDataTimestamp = time,
      blockDataExtraData = 0,
      blockDataMixHash = SHA 0,
      blockDataNonce = 5
      }
    }
