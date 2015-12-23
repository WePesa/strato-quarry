{-# LANGUAGE OverloadedStrings #-}

import Blockchain.Data.BlockDB
import Blockchain.DBM
import Control.Monad
import Database.PostgreSQL.Simple

import Trigger
import Wrapper

-- Actually much more general, see DBM.hs for the necessary constraints
instance HasSQLDB IO where
  getSQLDB = sqlDB' <$> openDBs

main = do
  conn <- connectPostgreSQL connStr
  setupTrigger conn
  forever $ do
    newBlock <- waitGetBestBlock conn
    _ <- putBlocks [newBlock]

connStr = "host=localhost dbname=eth user=postgres password=api port=" ++ show (port $ sqlConfig ethConf)
