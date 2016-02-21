{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module BlockConstruction where

import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import Blockchain.Data.Transaction
import Blockchain.Database.MerklePatricia hiding (Key)
import Blockchain.DB.SQLDB
import Blockchain.EthConf
import Blockchain.SHA

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource

import Data.Time.Clock

import Database.Esqueleto
import Database.Persist.Sql ()

import PersistSQL
import Debug

makeNewBlock :: (HasSQLDB (ResourceT m), MonadThrow m, MonadBaseControl IO m, MonadIO m) => SqlPersistT m Block
makeNewBlock = do
  newBest <- getBestBlock
  txs <- getGreenTXs newBest
  b <- constructBlock newBest txs
  if not . null $ txs
    then do
      putBlock b
      debugPrints [
        startDebugBlock, "Inserted block ", show $ blockDataNumber $ blockBlockData b,
        startDebugBlockLine, "Parent hash: ", showHash $ blockDataParentHash $ blockBlockData b,
        startDebugBlockLine, "(Fake) hash: ", showHash $ blockHash b,
        startDebugBlockLine, "Including transactions: ", showTXHashes b,
        endDebugBlock
        ]
    else do
      debugPrint "Empty block; not committing\n"
  return b
  where putBlock x = lift $ runResourceT $ head <$> putBlocks [x] True

constructBlock :: (MonadIO m) => Entity Block -> [Transaction] -> SqlPersistT m Block
constructBlock parentE txs = do
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
