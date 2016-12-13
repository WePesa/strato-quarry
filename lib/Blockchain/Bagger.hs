{-# LANGUAGE OverloadedStrings #-}
module Blockchain.Bagger where

import Control.Monad.Extra
import Control.Monad.IO.Class

import Data.Time.Clock
import Data.Maybe (isJust, fromJust)
import Numeric (readHex)

import Blockchain.DB.MemAddressStateDB
import Blockchain.DB.StateDB
import Blockchain.DB.HashDB

import Blockchain.Data.Address
import Blockchain.Database.MerklePatricia (StateRoot(..))
import Blockchain.SHA
import Blockchain.Sequencer.Event (OutputTx(..), OutputBlock(..), outputBlockHash)
import qualified Blockchain.Data.BlockDB as BDB
import qualified Blockchain.Data.DataDefs as DD
import qualified Blockchain.Data.TransactionDef as TD
import qualified Blockchain.Data.TXOrigin as TO
import qualified Blockchain.Verification as V
import qualified Blockchain.EthConf as Conf

import qualified Blockchain.Bagger.BaggerState as B
import Blockchain.Bagger.TransactionList

import qualified Data.Map as M
import qualified Data.Set as S

data RunAttemptError = CantFindStateRoot | GasLimitReached [OutputTx] deriving Show

class (Monad m, HasHashDB m, HasStateDB m, HasMemAddressStateDB m) => MonadBagger m where
    getBaggerState    :: m B.BaggerState
    putBaggerState    :: B.BaggerState -> m ()
    runFromStateRoot  :: StateRoot -> [OutputTx] -> m (Either RunAttemptError StateRoot) -- todo: should this be (StateRoot, [accepted_/rejected_txs])?
    {-# MINIMAL getBaggerState, putBaggerState, runFromStateRoot #-}

    updateBaggerState :: (B.BaggerState -> B.BaggerState) -> m ()
    updateBaggerState f = putBaggerState =<< (f <$> getBaggerState)

    addTransactionsToMempool :: [OutputTx] -> m ()
    addTransactionsToMempool ts = do
        existingStateDbStateRoot <- getStateRoot
        stateRoot <- B.lastExecutedStateRoot . B.miningCache <$> getBaggerState
        setStateDBStateRoot stateRoot
        sequence_ (addToQueued <$> ts)
        promoteExecutables
        setStateDBStateRoot existingStateDbStateRoot

    processNewBestBlock :: SHA -> DD.BlockData -> m ()
    processNewBestBlock blockHash bd = do
        existingStateDbStateRoot <- getStateRoot
        let thisStateRoot   = DD.blockDataStateRoot bd
        state <- getBaggerState
        let newMiningCache = B.MiningCache { B.bestBlockSHA          = blockHash
                                           , B.bestBlockHeader       = bd
                                           , B.lastExecutedStateRoot = thisStateRoot
                                           , B.lastExecutedTxs       = S.empty
                                           , B.promotedTransactions  = S.empty
                                           }
        putBaggerState $ state { B.miningCache = newMiningCache }
        setStateDBStateRoot thisStateRoot
        demoteUnexecutables
        promoteExecutables
        setStateDBStateRoot existingStateDbStateRoot

    makeNewBlock :: m OutputBlock
    makeNewBlock = do
        state <- getBaggerState
        let seen'       = B.seen state
        let cache       = B.miningCache state
        let lastExec    = B.lastExecutedTxs cache
        let lastExecLen = S.size lastExec
        let noCachedTxsCulled = lastExecLen == length [t | t <- S.toList lastExec, otHash t `M.member` seen']
        if noCachedTxsCulled
            then if (S.null $ B.promotedTransactions cache)
                then buildFromMiningCache
                else do
                    existingStateDbStateRoot <- getStateRoot
                    let lastSR    = B.lastExecutedStateRoot cache
                    let promoted  = B.promotedTransactions cache
                    let promoted' = S.toList promoted
                    run <- runFromStateRoot lastSR promoted'
                    case run of
                        Left e      -> error $ show e
                        Right newSR -> do
                            let unionLastExecAndPromoted = S.union lastExec promoted
                            let newMiningCache = cache { B.lastExecutedStateRoot = newSR
                                                       , B.lastExecutedTxs       = unionLastExecAndPromoted
                                                       , B.promotedTransactions  = S.empty
                                                       }
                            updateBaggerState (\s -> s { B.miningCache = newMiningCache })
                    setStateDBStateRoot existingStateDbStateRoot
                    buildFromMiningCache
            else do -- some transactions which were cached have been evicted, need to recalculate entire block cache
                let sha    = B.bestBlockSHA cache
                let header = B.bestBlockHeader cache
                processNewBestBlock sha header
                makeNewBlock


    setCalculateIntrinsicGas :: (Integer -> OutputTx -> Integer) -> m ()
    setCalculateIntrinsicGas cig = putBaggerState =<< (\s -> s { B.calculateIntrinsicGas = cig }) <$> getBaggerState

addToQueued :: MonadBagger m => OutputTx -> m ()
addToQueued t = unlessM (wasSeen t) $ -- unlikely due to sequencer, but theoretically possible
                    whenM (isValidForPool t) $ do
                        (toDiscard, newState) <- B.addToQueued t <$> getBaggerState
                        putBaggerState newState
                        forM_ toDiscard removeFromSeen
                        addToSeen t

promoteExecutables :: MonadBagger m => m ()
promoteExecutables = do
    state <- getBaggerState
    let queued' = M.keysSet $ B.queued state
    forM_ queued' $ \address -> do
        (addressNonce, addressBalance) <- getAddressNonceAndBalance address

        let (discardedByNonce, state) = B.trimBelowNonceFromQueued address addressNonce state
        putBaggerState state
        forM_ discardedByNonce removeFromSeen

        let (discardedByCost, state) = B.trimAboveCostFromQueued address addressBalance state
        putBaggerState state
        forM_ discardedByCost removeFromSeen

        let (readyToMine, state) = B.popSequentialFromQueued address addressNonce state
        putBaggerState state
        forM_ readyToMine promoteTx

promoteTx :: MonadBagger m => OutputTx -> m ()
promoteTx tx = do
    state <- getBaggerState
    let (evicted, state) = B.addToPending tx state
    putBaggerState state
    forM_ evicted removeFromSeen
    addToPromotionCache tx

demoteUnexecutables :: MonadBagger m => m ()
demoteUnexecutables = do
    state <- getBaggerState
    let pending' = M.keysSet $ B.pending state
    forM_ pending' $ \address -> do
        (addressNonce, addressBalance) <- getAddressNonceAndBalance address

        let (discardedByNonce, state) = B.trimBelowNonceFromPending address addressNonce state
        putBaggerState state
        forM_ discardedByNonce removeFromSeen

        let (discardedByCost, state) = B.trimAboveCostFromPending address addressBalance state
        putBaggerState state
        forM_ discardedByCost removeFromSeen

        -- drop all existing pending transactions, and try to see if they're
        -- still valid to add to the (likely new) queued pool
        let (remainingPending, state) = B.popAllPending state
        putBaggerState state
        forM_ remainingPending addToQueued

wasSeen :: MonadBagger m => OutputTx -> m Bool
wasSeen OutputTx{otHash=sha} = (M.member sha) . B.seen <$> getBaggerState

isValidForPool :: MonadBagger m => OutputTx -> m Bool
isValidForPool t@OutputTx{otSigner=address, otBaseTx=bt} = do
    -- todo: is this everything that can be checked? be more pedantic and check for neg. balance, etc?
    state <- getBaggerState
    let txFee = B.calculateIntrinsicTxFee state t
    (addressNonce, addressBalance) <- getAddressNonceAndBalance address
    return $ (addressNonce > (TD.transactionNonce bt)) && (addressBalance >= txFee)

addToSeen :: MonadBagger m => OutputTx -> m ()
addToSeen t = updateBaggerState (B.addToSeen t)

removeFromSeen :: MonadBagger m => OutputTx -> m ()
removeFromSeen t = updateBaggerState (B.removeFromSeen t)

getAddressNonceAndBalance :: MonadBagger m => Address -> m (Integer, Integer)
getAddressNonceAndBalance addr = (\aS -> (DD.addressStateNonce aS, DD.addressStateBalance aS)) <$> getAddressState addr

addToPromotionCache :: MonadBagger m => OutputTx -> m ()
addToPromotionCache tx = updateBaggerState $ B.addToPromotionCache tx

buildFromMiningCache :: MonadBagger m => m OutputBlock
buildFromMiningCache = do
    time <- liftIO $ getCurrentTime
    cache <- B.miningCache <$> getBaggerState
    let uncles       = []
    let parentHash   = B.bestBlockSHA cache
    let parentHeader = B.bestBlockHeader cache
    let stateRoot    = B.lastExecutedStateRoot cache
    let txs          = S.toList $ B.lastExecutedTxs cache
    let parentDiff   = DD.blockDataDifficulty parentHeader
    let nextDiff     = BDB.nextDifficulty False (DD.blockDataNumber parentHeader) parentDiff (DD.blockDataTimestamp parentHeader) time
    return OutputBlock { obOrigin = TO.Quarry
                       , obTotalDifficulty = parentDiff + nextDiff
                       , obBlockUncles = uncles
                       , obReceiptTransactions = txs
                       , obBlockData = DD.BlockData {
                             DD.blockDataParentHash       = parentHash
                           , DD.blockDataUnclesHash       = V.ommersVerificationValue uncles
                           , DD.blockDataCoinbase         = fromInteger . fst . head . readHex . Conf.coinbaseAddress . Conf.quarryConfig $ Conf.ethConf
                           , DD.blockDataStateRoot        = stateRoot
                           , DD.blockDataTransactionsRoot = V.transactionsVerificationValue (otBaseTx <$> txs)
                           , DD.blockDataReceiptsRoot     = V.receiptsVerificationValue ()
                           , DD.blockDataLogBloom         = "0000000000000000000000000000000000000000000000000000000000000000"
                           , DD.blockDataDifficulty       = nextDiff
                           , DD.blockDataNumber           = DD.blockDataNumber parentHeader + 1
                           , DD.blockDataGasLimit         = let g     = DD.blockDataGasLimit parentHeader
                                                                (q,d) = g `quotRem` 1024
                                                                in g + q - (if d == 0 then 1 else 0)
                           , DD.blockDataGasUsed = 0
                           , DD.blockDataTimestamp = time
                           , DD.blockDataExtraData = 0
                           , DD.blockDataMixHash = SHA 0
                           , DD.blockDataNonce = 5
                           }
                       }


