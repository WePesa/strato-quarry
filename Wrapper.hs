{-# LANGUAGE OverloadedStrings #-}
module Wrapper (makeBlock) where

import Blockchain.Data.Address
import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import Blockchain.Data.Transaction
import Blockchain.Database.MerklePatricia
import Blockchain.SHA
import Blockchain.Verifier

import Control.Monad.Logger

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import Data.Time.Clock

import Database.Esqueleto hiding (Connection)
import Database.Persist.Postgresql (withPostgresqlConn, runSqlConn)
import Database.PostgreSQL.Simple

import Network.Haskoin.Internals (makePrvKey, PrvKey)

import Trigger

makeBlock :: Connection -> IO Block
makeBlock conn = do
  parentE <- waitGetBestBlock conn
  let parent = entityVal parentE
      parentData = blockBlockData parent
  uncles <- getSiblings conn parent
  txs <- getGreenTXs conn parentE
  time <- getCurrentTime
  return $ Block {
    blockBlockUncles = uncles,
    blockReceiptTransactions = txs,
    blockBlockData = BlockData {
      blockDataParentHash = blockHash parent,
      blockDataUnclesHash = hash . rlpSerialize . RLPArray $ map rlpEncode uncles,
      blockDataCoinbase = prvKey2Address ourPrvKey,
      blockDataStateRoot = SHAPtr "",
      blockDataTransactionsRoot = emptyTriePtr, -- FIXME
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

getSiblings :: Connection -> Block -> IO [BlockData]
getSiblings conn Block{blockBlockData = BlockData{blockDataParentHash = pHash}} =
  let connStr = postgreSQLConnectionString defaultConnectInfo
  in runNoLoggingT $ withPostgresqlConn connStr $ runSqlConn $ do
    blocks <-
      select $
      from $ \(block `InnerJoin` blockDR) -> do
        on (blockDR ^. BlockDataRefBlockId ==. block ^. BlockId &&.
            blockDR ^. BlockDataRefParentHash ==. val pHash)
        return block
    return $ map (blockBlockData . entityVal) blocks

getGreenTXs :: Connection -> Entity Block -> IO [Transaction]
getGreenTXs conn blockE = do
  let connStr = postgreSQLConnectionString defaultConnectInfo
  runNoLoggingT $ withPostgresqlConn connStr $ runSqlConn $ do
    earliest:_ <- do
      txs <-
        select $
        from $ \rawTX -> do
          on (rawTX ^. RawTransactionBlockId ==. val (entityKey blockE))
          orderBy [asc $ rawTX ^. RawTransactionTimestamp]
          limit 1
          return rawTX
      return $ map (rawTransactionTimestamp . entityVal) txs
    let startTime = addUTCTime (negate timeRadius) earliest
    laterBlockEs <-
      select $
      from $ \(block `InnerJoin` blockDR) -> do
        on (blockDR ^. BlockDataRefBlockId ==. block ^. BlockId &&.
            blockDR ^. BlockDataRefTimestamp >. val startTime)
        return block
    let recentBlockEMap = Map.fromList $ do
          recentBlockE@(Entity blockId block) <- laterBlockEs
          return (blockHash block, recentBlockE)
        recentChain =
          catMaybes $ List.takeWhile isJust $
          Just blockE :
          map (
            flip Map.lookup recentBlockEMap .
            blockDataParentHash .
            blockBlockData .
            entityVal
            )
          recentChain
        recentChainIds = map entityKey recentChain
    rawtxs <-
      select $
      from $ \rawTX -> do
        on $ (rawTX ^. RawTransactionTimestamp >. val startTime) &&.
          (foldr1 (&&.) $
           map (\id -> rawTX ^. RawTransactionBlockId !=. val id) recentChainIds)
        return rawTX
    return $ map (rawTX2TX . entityVal) rawtxs

ourPrvKey = fromJust $ makePrvKey
            0x0000000000000000000000000000000000000000000000000000000000000000
            :: PrvKey

timeRadius = 60 :: NominalDiffTime -- seconds
