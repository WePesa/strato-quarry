{-# LANGUAGE ScopedTypeVariables #-}

module PersistSQL where

import Blockchain.Data.BlockDB
import Blockchain.Data.DataDefs
import Blockchain.Data.Transaction
import Blockchain.Database.MerklePatricia (SHAPtr)

import Control.Monad.IO.Class

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Time.Clock

import Database.Esqueleto
import Database.Persist.Sql ()

type BlockIds = (Key Block, Key BlockDataRef)

deleteBlockQ :: (MonadIO m) => BlockIds -> SqlPersistT m ()
deleteBlockQ (bId, bdId) = do
  delete $ from $ \p -> where_ (p ^. UnprocessedBlockId ==. val bId)
  delete $ from $ \b -> where_ (b ^. BlockDataRefId ==. val bdId)
  delete $ from $ \b -> where_ (b ^. BlockId ==. val bId)
  return ()

getSiblings :: (MonadIO m) => Block -> SqlPersistT m [BlockData]
getSiblings Block{blockBlockData = BlockData{blockDataParentHash = pHash}} = do
  blocks <-
    select $
    from $ \(block `InnerJoin` blockDR) -> do
      on (blockDR ^. BlockDataRefBlockId ==. block ^. BlockId &&.
          blockDR ^. BlockDataRefParentHash ==. val pHash)
      return block
  return $ map (blockBlockData . entityVal) blocks

getGreenTXs :: (MonadIO m) => Entity Block -> SqlPersistT m [Transaction]
getGreenTXs blockE = do
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
        recentBlockE@(Entity{entityVal = block}) <- laterBlockEs
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
      recentTXs = concatMap (blockReceiptTransactions . entityVal) recentChain
  rawtxs <-
    select $
    from $ \rawTX -> do
      where_ $ (rawTX ^. RawTransactionTimestamp >. val startTime)
      return $ rawTX
  let greenTXs = Set.toList $
                 (Set.fromList $ map (rawTX2TX . entityVal) rawtxs) Set.\\
                 (Set.fromList recentTXs)        
  return greenTXs

  where timeRadius = 60 :: NominalDiffTime -- seconds

getBestBlock :: (MonadIO m) => SqlPersistT m (Entity Block)
getBestBlock = do
  s <- getJust (ExtraKey "bestBlock")
  let (bId, _::Integer) = read $ extraValue s
  head <$> (select $ from $ \b -> do
               where_ (b ^. BlockId ==. val bId)
               return b)

getNewTransactions :: (MonadIO m) => SqlPersistT m [Transaction]
getNewTransactions = do
  rawtxEs <-
    select $
    from $ \rawtx -> do
      where_ (rawtx ^. RawTransactionBlockNumber ==. val (-1))
      return rawtx
  return $ map (rawTX2TX . entityVal) rawtxEs
