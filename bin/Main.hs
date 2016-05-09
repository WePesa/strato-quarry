{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}

import Blockchain.EthConf ()
import Blockchain.Output
import Blockchain.Stream.VMEvent
import Blockchain.Quarry
import Blockchain.Quarry.Flags ()
import Blockchain.Quarry.SQL.Conn
import Control.Monad
import Control.Monad.Logger
import HFlags
import System.IO

lMain::LoggingT IO ()
lMain = do
  runConnT $ do
    asPersistTransaction makeNewBlock
    asSimpleTransaction setupTriggers
    forever $ do
      produceVMEvents [NewUnminedBlockAvailable]
      waitNotification

main = do
  _ <- $initHFlags "Block builder for the Haskell EVM"
  flip runLoggingT printLogMsg lMain
