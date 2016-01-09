{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}

import Blockchain.EthConf
import Blockchain.VMOptions
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Database.PostgreSQL.Simple
import HFlags
import System.IO

import SQLMonad
import Trigger
import Wrapper

main = do
  _ <- $initHFlags "Block builder for the Haskell EVM"
  
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  
  withConnInfo
    ConnectInfo {
      connectHost = "localhost",
      connectPort = fromIntegral $ port $ sqlConfig ethConf,
      connectUser = "postgres",
      connectPassword = "api",
      connectDatabase = "eth"
      } $
    do
      liftIO $ putStrLn "Creating a seed block"
      seedDBBlock <- makeNewBlock
      setupTriggers
      evalStateT (forever waitMakeBlock) seedDBBlock
