module Blockchain.Bagger.BaggerState where

import Control.Applicative (Alternative, empty)

import Blockchain.Bagger.TransactionList
import Blockchain.Sequencer.Event (OutputTx(..))

import Blockchain.Data.Address
import qualified Blockchain.Data.TransactionDef as TD
import Blockchain.SHA

import qualified Data.Map as M

type ATL = M.Map Address TransactionList

data BaggerState = BaggerState { bestBlockSHA          :: SHA
                               , pending               :: ATL -- TXs that are going in the next block
                               , queued                :: ATL -- TXs that are lingering in the pool
                               , seen                  :: M.Map SHA OutputTx
                               , calculateIntrinsicGas :: OutputTx -> Integer -- fn that calculates intrinsic gas cost for a given Tx
                               }

defaultBaggerState :: BaggerState
defaultBaggerState = BaggerState { bestBlockSHA          = SHA 0
                                 , pending               = M.empty
                                 , queued                = M.empty
                                 , seen                  = M.empty
                                 , calculateIntrinsicGas = error "wyd bro"
                                 }

addToATL :: OutputTx -> ATL -> (Maybe OutputTx, ATL)
addToATL t atl =
    case (M.lookup signer atl) of
        Nothing  -> (Nothing, M.insert signer (singletonTransactionList t) atl)
        Just txs -> let (oldTx, newTL) = (insertTransaction t txs) in (oldTx, M.insert signer newTL atl)
    where signer = otSigner t

modifyATL :: Alternative a => (TransactionList -> (a OutputTx, TransactionList)) -> Address -> ATL -> (a OutputTx, ATL)
modifyATL f address atl = case (M.lookup address atl) of
    Nothing -> (empty, atl)
    Just tl -> let (poppedTx, newTL) = (f tl) in
        if (M.null newTL)
            then (poppedTx, M.delete address atl)
            else (poppedTx, M.insert address newTL atl)

calculateIntrinsicTxFee :: BaggerState -> (OutputTx -> Integer)
calculateIntrinsicTxFee bs t@OutputTx{otBaseTx = bt} = (TD.transactionGasPrice bt) * ((calculateIntrinsicGas bs) t)

addToPending :: OutputTx -> BaggerState -> (Maybe OutputTx, BaggerState)
addToPending t s@BaggerState{pending = p} = let (oldTx, newATL) = (addToATL t p) in (oldTx, s { pending = newATL })

addToQueued :: OutputTx -> BaggerState -> (Maybe OutputTx, BaggerState)
addToQueued t s@BaggerState{queued = q} = let (oldTx, newATL) = (addToATL t q) in (oldTx, s { queued = newATL })

addToSeen :: OutputTx -> BaggerState -> BaggerState
addToSeen t@OutputTx{otHash=sha} s@BaggerState{seen = seen'} = s { seen = (M.insert sha t seen') }

removeFromSeen :: OutputTx -> BaggerState -> BaggerState
removeFromSeen OutputTx{otHash=sha} s@BaggerState{seen = seen'} = s { seen = (M.delete sha seen') }

trimBelowNonceFromQueued :: Address -> Integer -> BaggerState -> ([OutputTx], BaggerState)
trimBelowNonceFromQueued a nonce s@BaggerState{queued = q} =
    let (oldTX, newATL) = modifyATL (trimBelowNonce nonce) a q in (oldTX, s { queued = newATL })

trimBelowNonceFromPending :: Address -> Integer -> BaggerState -> ([OutputTx], BaggerState)
trimBelowNonceFromPending a nonce s@BaggerState{pending = p} =
    let (oldTx, newATL) = modifyATL (trimBelowNonce nonce) a p in (oldTx, s { pending = newATL })

trimAboveCostFromQueued :: Address -> Integer -> BaggerState -> ([OutputTx], BaggerState)
trimAboveCostFromQueued a maxCost s@BaggerState{queued = q} =
    let (oldTX, newATL) = modifyATL (trimAboveCost maxCost (calculateIntrinsicTxFee s)) a q in
        (oldTX, s { queued = newATL })

trimAboveCostFromPending :: Address -> Integer -> BaggerState -> ([OutputTx], BaggerState)
trimAboveCostFromPending a maxCost s@BaggerState{pending = p} =
    let (oldTX, newATL) = modifyATL (trimAboveCost maxCost (calculateIntrinsicTxFee s)) a p in
        (oldTX, s { pending = newATL })

popSequentialFromQueued :: Address -> Integer -> BaggerState -> ([OutputTx], BaggerState)
popSequentialFromQueued a nonce s@BaggerState{queued = q} =
    let (popped, newATL) = modifyATL (popSequential nonce) a q in (popped, s { queued = newATL })
