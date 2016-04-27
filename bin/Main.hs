{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}

import Blockchain.EthConf ()
import Blockchain.Stream.VMEvent
import Blockchain.Quarry
import Blockchain.Quarry.Flags ()
import Blockchain.Quarry.SQL.Conn
import Control.Monad
import HFlags
import System.IO

main = do
  _ <- $initHFlags "Block builder for the Haskell EVM"

  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  
  runConnT $ do
    asPersistTransaction makeNewBlock
    asSimpleTransaction setupTriggers
    forever $ do
      waitNotification
      produceVMEvents [NewUnminedBlockAvailable]
