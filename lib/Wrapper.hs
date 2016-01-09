module Wrapper where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import SQLMonad
import SimpleSQL
import BlockConstruction

waitMakeBlockState :: StateT DBBlock ConnT ()
waitMakeBlockState = modifyM $ lift . waitMakeNextBlock
  where modifyM :: (Monad m) => (s -> StateT s m s) -> StateT s m ()
        modifyM f = put =<< f =<< get                    

waitMakeNextBlock :: DBBlock -> ConnT DBBlock
waitMakeNextBlock oldDBBlock = do
  channel <- waitNotifyData
  asPersistTransaction $ case channel of
    QuarryNewTX -> updateBlock oldDBBlock
    QuarryBestBlock -> makeNewBlock
