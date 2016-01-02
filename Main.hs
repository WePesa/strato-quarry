{-# LANGUAGE FlexibleInstances #-}

import Blockchain.Data.BlockDB
import Blockchain.DB.SQLDB
import Blockchain.DBM
import Blockchain.EthConf
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Database.PostgreSQL.Simple

import Data.ByteString.Char8 (pack)

import Trigger
import Wrapper

-- Actually much more general, see DBM.hs for the necessary constraints
instance HasSQLDB (ResourceT IO) where
  getSQLDB = sqlDB' <$> openDBs

main = do
  conn <- connectPostgreSQL connStr
  setupTrigger conn
  runResourceT $ forever $ do
    newBlock <- liftIO $ makeBlock conn
    putBlocks [newBlock]

connStr = pack $ "host=localhost dbname=eth user=postgres password=api port=" ++ show (port $ sqlConfig ethConf)
