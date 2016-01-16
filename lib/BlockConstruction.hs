{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module BlockConstruction where

import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import Blockchain.Data.Transaction
import Blockchain.Database.MerklePatricia hiding (Key)
import Blockchain.DB.SQLDB
import Blockchain.EthConf
import Blockchain.Format ()
import Blockchain.SHA

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource

import Data.Time.Clock

import Database.Esqueleto
import Database.Persist.Sql ()

import PersistSQL
import Debug

data DBBlock = DBBlock {
  dbBlock :: Block,
  dbBlockIds :: Maybe BlockIds
  }

updateBlock :: (HasSQLDB (ResourceT m), MonadThrow m, MonadBaseControl IO m, MonadIO m) => DBBlock -> SqlPersistT m DBBlock
updateBlock oldDBBlock = do
  liftIO $ putStrLn "New transactions: update previous block"
  let oldTXs = blockReceiptTransactions $ dbBlock oldDBBlock
  txs <- getNewTransactions
  newBlock <- constructBlock $ oldTXs ++ txs
  bids <- makeBlockIds newBlock
  maybe
    (debugPrints [
        startDebugBlockLine, "(New block)",
        endDebugBlock
        ]
    )
    (
      \oldBIds -> debugPrints [
        startDebugBlockLine, "Updating block: ", showBlockIds oldBIds,
        endDebugBlock
        ]
    )
    $ dbBlockIds oldDBBlock
  return DBBlock {
    dbBlock = newBlock,
    dbBlockIds = Just bids
    }

makeNewBlock :: (HasSQLDB (ResourceT m), MonadThrow m, MonadBaseControl IO m, MonadIO m) => SqlPersistT m DBBlock
makeNewBlock = do
  liftIO $ putStrLn "New best block: making a new block"
  newBest <- getBestBlock
  txs <- getGreenTXs newBest
  b <- constructBlock txs
  bidsM <-
    if not . null $ blockReceiptTransactions b
    then do
      r <- Just <$> makeBlockIds b
      debugPrints [
        startDebugBlockLine, "(New block)",
        endDebugBlock
        ]
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
  bids <- putBlock b
  debugPrints [
    startDebugBlock,
    startDebugBlockLine, "Inserted block ", showBlockIds bids,
    startDebugBlockLine, "Including transactions: ", showTXHashes b
    ]
  return bids

  where putBlock x = lift $ runResourceT $ head <$> putBlocks [x] True

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

constructBlock :: (MonadIO m) => [Transaction] -> SqlPersistT m Block
constructBlock txs = do
  parentE <- getBestBlock
  let parent = entityVal parentE
      parentData = blockBlockData parent
  parentHash:_ <- select $ from $ \bdr -> do
    where_ (bdr ^. BlockDataRefBlockId ==. val (entityKey parentE))
    return $ bdr ^. BlockDataRefHash
  uncles <- getSiblings parent
  time <- liftIO getCurrentTime
  return $ Block {
    blockBlockUncles = uncles,
    blockReceiptTransactions = txs,
    blockBlockData = BlockData {
      blockDataParentHash = unValue parentHash,
      blockDataUnclesHash = hash . rlpSerialize . RLPArray $ map rlpEncode uncles,
      blockDataCoinbase = fromIntegral $ coinbaseAddress $ quarryConfig ethConf,
      blockDataStateRoot = SHAPtr "",
      blockDataTransactionsRoot = emptyTriePtr,
      blockDataReceiptsRoot = emptyTriePtr,
      blockDataLogBloom =
        "0000000000000000000000000000000000000000000000000000000000000000",
      blockDataDifficulty =
        nextDifficulty
        False
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
