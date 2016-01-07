{-# LANGUAGE FlexibleInstances #-}

import Blockchain.EthConf
import Control.Monad
import Control.Monad.Trans.State
import Database.PostgreSQL.Simple

import SQLMonad
import Trigger
import Wrapper

main =
  withConnInfo
  ConnectInfo {
    connectHost = "localhost",
    connectPort = fromIntegral $ port $ sqlConfig ethConf,
    connectUser = "postgres",
    connectPassword = "api",
    connectDatabase = "eth"
    } $
  do
    best <- getBestBlock
    seedBlock <- constructBlock best []
    setupTriggers
    evalStateT (forever makeBlock) seedBlock
