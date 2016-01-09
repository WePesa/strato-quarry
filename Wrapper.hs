{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Wrapper (waitMakeBlock, makeNewBlock) where

import Blockchain.Data.Address
import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import Blockchain.Data.Transaction
import Blockchain.Database.MerklePatricia hiding (Key)
import Blockchain.SHA
import Blockchain.Verifier

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import Data.Time.Clock

import Database.Esqueleto hiding (Connection, get)
import Database.Persist.Postgresql (withPostgresqlConn, runSqlConn)
import Database.PostgreSQL.Simple

import Network.Haskoin.Internals (makePrvKey, PrvKey)

import Trigger
import SQLMonad

import Debug.Trace

type BlockIds = (Key Block, Key BlockDataRef)
data DBBlock = DBBlock {
  dbBlock :: Block,
  dbBlockIds :: Maybe BlockIds
  }

waitMakeBlock :: StateT DBBlock ConnT ()
waitMakeBlock = modifyM $ lift . waitMakeNextBlock

modifyM :: (Monad m) => (s -> StateT s m s) -> StateT s m ()
modifyM f = put =<< f =<< get                    

waitMakeNextBlock :: DBBlock -> ConnT DBBlock
waitMakeNextBlock oldDBBlock = do
  channel <- waitNotifyData
  case channel of
    QuarryNewTX -> updateBlock oldDBBlock
    QuarryBestBlock -> makeNewBlock

updateBlock :: DBBlock -> ConnT DBBlock
updateBlock oldDBBlock = do
  liftIO $ putStrLn "New transactions: update previous block"
  txs <- getNewTransactions
  let oldBlock = dbBlock oldDBBlock
      oldBIdsM = dbBlockIds oldDBBlock
      oldTXs = blockReceiptTransactions oldBlock
      newTXs = oldTXs ++ txs
      b = oldBlock { blockReceiptTransactions = newTXs }
  bids <- makeBlockIds b
  maybe (return ()) (asPersistTransaction . deleteBlockQ) oldBIdsM
  return DBBlock {
    dbBlock = b,
    dbBlockIds = Just bids
    }

deleteBlockQ :: BlockIds -> SqlPersistT ConnT ()
deleteBlockQ (bId, bdId) = do
  delete $ from $ \p -> where_ (p ^. UnprocessedBlockId ==. val bId)
  delete $ from $ \b -> where_ (b ^. BlockDataRefId ==. val bdId)
  delete $ from $ \b -> where_ (b ^. BlockId ==. val bId)
  return ()

makeNewBlock :: ConnT DBBlock
makeNewBlock = do
  liftIO $ putStrLn "New best block: making a new block"
  newBest <- getBestBlock
  txs <- getGreenTXs newBest
  b <- constructBlock newBest txs
  bidsM <-
    if not . null $ blockReceiptTransactions b
    then Just <$> makeBlockIds b
    else do
      liftIO $ putStrLn "Empty block; not committing"
      return Nothing
  return DBBlock {
    dbBlock = b,
    dbBlockIds = bidsM
    }

makeBlockIds :: Block -> ConnT BlockIds
makeBlockIds b = do
  liftIO $ putStrLn "Committing the block to the database"
  asPersistTransaction $ putBlock b

constructBlock :: Entity Block -> [Transaction] -> ConnT Block
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
      blockDataCoinbase = prvKey2Address ourPrvKey,
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

    where ourPrvKey = fromJust $ makePrvKey 57 :: PrvKey -- Grothendieck prime

getSiblings :: Block -> ConnT [BlockData]
getSiblings Block{blockBlockData = BlockData{blockDataParentHash = pHash}} =
  asPersistTransaction $ do
    blocks <-
      select $
      from $ \(block `InnerJoin` blockDR) -> do
        on (blockDR ^. BlockDataRefBlockId ==. block ^. BlockId &&.
            blockDR ^. BlockDataRefParentHash ==. val pHash)
        return block
    return $ map (blockBlockData . entityVal) blocks

getGreenTXs :: Entity Block -> ConnT [Transaction]
getGreenTXs blockE =
  asPersistTransaction $ do
    earliest:_ <- do
      txs <-
        select $
        from $ \rawTX -> do
          where_ (rawTX ^. RawTransactionBlockId ==. val (entityKey blockE))
          orderBy [asc $ rawTX ^. RawTransactionTimestamp]
          limit 1
          return rawTX
      return $
        if null txs
        then [blockDataTimestamp . blockBlockData . entityVal $ blockE]
        else map (rawTransactionTimestamp . entityVal) txs

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
        where_ $ (rawTX ^. RawTransactionTimestamp >. val startTime) &&.
          (foldr1 (&&.) $
           map (\id -> rawTX ^. RawTransactionBlockId !=. val id) recentChainIds)
        return rawTX
    return $ map (rawTX2TX . entityVal) rawtxs

  where timeRadius = 60 :: NominalDiffTime -- seconds

getBestBlock :: ConnT (Entity Block)
getBestBlock = do
  Just (Entity {entityVal = Extra {extraValue = eV}}) <-
    asPersistTransaction $ getBy (TheKey "bestBlockNumber")
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
