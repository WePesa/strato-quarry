module Blockchain.Bagger where

import Control.Monad.Extra

import Blockchain.DB.MemAddressStateDB
import Blockchain.DB.StateDB
import Blockchain.DB.HashDB


import Blockchain.Data.Address
import Blockchain.Database.MerklePatricia (StateRoot(..))
import qualified Blockchain.Data.DataDefs as DD
import qualified Blockchain.Data.TransactionDef as TD
import Blockchain.Data.DataDefs (AddressState(..), blockDataStateRoot)
import Blockchain.SHA
import Blockchain.Sequencer.Event (OutputTx(..), OutputBlock(..), outputBlockHash)

import qualified Blockchain.Bagger.BaggerState as B
import Blockchain.Bagger.TransactionList

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (isJust, fromJust)

data RunAttemptError = CantFindStateRoot | GasLimitReached [OutputTx]

class (Monad m, HasHashDB m, HasStateDB m, HasMemAddressStateDB m) => MonadBagger m where
    getBaggerState    :: m B.BaggerState
    putBaggerState    :: B.BaggerState -> m ()
    runFromStateRoot  :: StateRoot -> [OutputTx] -> m (Either RunAttemptError StateRoot) -- todo: should this be (StateRoot, [accepted_/rejected_txs])?
    {-# MINIMAL getBaggerState, putBaggerState, runFromStateRoot #-}

    addTransactionsToMempool :: [OutputTx] -> m ()
    addTransactionsToMempool ts = sequence_ (addToQueued <$> ts) >> promoteExecutables

    processNewBestBlock :: OutputBlock -> m ()
    processNewBestBlock ob = do
        let thisBlockData   = obBlockData ob
        let thisStateRoot   = DD.blockDataStateRoot thisBlockData
        state <- getBaggerState
        let newMiningCache = B.MiningCache { B.bestBlockSHA          = outputBlockHash ob
                                           , B.bestBlockStateRoot    = thisStateRoot
                                           , B.bestBlockNumber       = DD.blockDataNumber thisBlockData
                                           , B.lastExecutedStateRoot = thisStateRoot
                                           , B.promotedTransactions  = S.empty
                                           }
        putBaggerState $ state { B.miningCache = Just newMiningCache }
        setStateDBStateRoot thisStateRoot
        demoteUnexecutables
        promoteExecutables

    makeNewBlock :: m OutputBlock
    makeNewBlock = error "todo makeNewBlock"

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
addToSeen t = putBaggerState =<< ((B.addToSeen t) <$> getBaggerState)

removeFromSeen :: MonadBagger m => OutputTx -> m ()
removeFromSeen t = putBaggerState =<< ((B.removeFromSeen t) <$> getBaggerState)

getAddressNonceAndBalance :: MonadBagger m => Address -> m (Integer, Integer)
getAddressNonceAndBalance addr = (\aS -> (addressStateNonce aS, addressStateBalance aS)) <$> getAddressState addr

