{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Blockchain.EthConf
import Blockchain.Quarry
import Blockchain.Quarry.BlockConstruction
import Blockchain.Quarry.Flags ()
import Blockchain.Quarry.SQL.Conn
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import HFlags
import System.IO

main = do
  _ <- $initHFlags "Block builder for the Haskell EVM"

  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  
  runConnT $ do
    liftIO $ putStrLn "Creating a seed block"
    seedDBBlock <- asPersistTransaction makeNewBlock
    liftIO $ putStrLn "Creating triggers and listeners"
    asSimpleTransaction setupTriggers
    evalStateT (forever waitMakeBlockState) seedDBBlock
