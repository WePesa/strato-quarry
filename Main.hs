{-# LANGUAGE FlexibleInstances #-}

import Blockchain.EthConf
import Control.Monad
import Database.PostgreSQL.Simple

import SQLMonad
import Wrapper

main =
  withConnInfo
  ConnectInfo {
    connectHost = "localhost",
    connectPort = show $ port $ sqlConfig ethConf,
    connectUser = "postgres",
    connectPassword = "api",
    connectDatabase = "eth"
    } $
  do
    best <- getBestBlock
    seedBlock <- constructBlock best []
    setupTriggers
    runStateT seedBlock $ forever makeBlock
