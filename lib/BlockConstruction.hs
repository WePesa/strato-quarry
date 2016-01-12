{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module BlockConstruction where

import Blockchain.Data.BlockDB
import Blockchain.Data.RLP
import Blockchain.Data.Transaction
import Blockchain.Database.MerklePatricia hiding (Key)
import Blockchain.DB.SQLDB
import Blockchain.EthConf
import Blockchain.SHA
import Blockchain.Verifier

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource

import Data.Time.Clock

import Database.Persist.Sql

import PersistSQL
import Debug

data DBBlock = DBBlock {
  dbBlock :: Block,
  dbBlockIds :: Maybe BlockIds
  }

updateBlock :: (HasSQLDB (ResourceT m), MonadThrow m, MonadBaseControl IO m, MonadIO m) => DBBlock -> SqlPersistT m DBBlock
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
        _ <-
          if (blockDataNonce $ blockBlockData $ dbBlock oldDBBlock) /= 5
          then debugPrint "\tNot replacing mined block "
          else do
            deleteBlockQ oldBIds
            debugPrint "\tReplacing block "
        debugPrint $
          "(bId: " ++ show bId ++ ", bdId: " ++ show bdId ++ ")\n" ++
          "---\n"
    )
    oldBIdsM
  return DBBlock {
    dbBlock = b,
    dbBlockIds = Just bids
    }

makeNewBlock :: (HasSQLDB (ResourceT m), MonadThrow m, MonadBaseControl IO m, MonadIO m) => SqlPersistT m DBBlock
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

makeBlockIds :: (HasSQLDB (ResourceT m), MonadThrow m, MonadBaseControl IO m, MonadIO m) => Block -> SqlPersistT m BlockIds
makeBlockIds b = do
  bids@(bId, bdId) <- putBlock b
  debugPrint $ "--- \n" ++
    "\tInserted block (bId: " ++ show bId ++ ", bdId: " ++ show bdId ++ ")\n" ++
    "\tBlock hash: " ++ show (blockHash b) ++ "\n" ++
    "\tIncluding transactions: \n" ++
    (concatMap (\t -> "\t\tTX Hash: " ++ show (transactionHash t) ++ "\n") $
     blockReceiptTransactions b)
  return bids

  where putBlock b = lift $ runResourceT $ head <$> putBlocks [b]

{- Proposed alternative definition of putBlock, for blockapps-data -}

-- putBlock :: (MonadIO m) =>
--             Block -> E.SqlPersistT m (Key Block, Key BlockDataRef)
-- putBlock b = do
--   blkId <- SQL.insert $ b
--   toInsert <- blk2BlkDataRef b blkId
--   time <- liftIO getCurrentTime
--   mapM_ (insertOrUpdate b blkId) ((map (\tx -> txAndTime2RawTX tx blkId (blockDataNumber (blockBlockData b)) time)  (blockReceiptTransactions b)))
--   blkDataRefId <- SQL.insert $ toInsert
--   _ <- SQL.insert $ Unprocessed blkId
--   return $ (blkId, blkDataRefId)

--   where insertOrUpdate b blkid rawTX  = do
--           (txId :: [Entity RawTransaction]) <- SQL.selectList [ RawTransactionTxHash SQL.==. (rawTransactionTxHash rawTX )] [ ]
--           case txId of
--             [] -> do
--               _ <- SQL.insert rawTX
--               return ()
--             lst -> mapM_ (\t -> SQL.update (SQL.entityKey t)
--                                 [ RawTransactionBlockId SQL.=. blkid,
--                                   RawTransactionBlockNumber SQL.=. (fromIntegral $ blockDataNumber (blockBlockData b)) ])
--                    lst

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
