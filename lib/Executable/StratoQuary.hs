{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

module Executable.StratoQuary (
  stratoQuary
  ) where

import Blockchain.EthConf ()
import Blockchain.Stream.VMEvent
import Blockchain.Quarry
import Blockchain.Quarry.Flags ()
import Blockchain.Quarry.SQL.Conn

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Concurrent

import API.StratoQuarry

stratoQuary::LoggingT IO ()
stratoQuary = do
  logInfoN "Starting Quarry HTTP Server"

  _ <- liftIO . forkIO $ stratoQuarryAPIMain

  runConnT $ do
    asPersistTransaction makeNewBlock
    asSimpleTransaction setupTriggers
    forever $ do
      produceVMEvents [NewUnminedBlockAvailable]
      waitNotification
