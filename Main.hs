{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

import Blockchain.DBM
import Blockchain.DB.SQLDB
import Blockchain.Data.Address
import Blockchain.Data.DataDefs
import Blockchain.Data.BlockDB
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Int
import Data.Maybe
import Database.Persist.Postgresql hiding (Connection)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import Network.Haskoin.Internals (makePrvKey)
import Data.ByteString.Char8 (unpack, pack)

import qualified Blockchain.Database.MerklePatricia as MP
import qualified Data.ByteString as B
import Blockchain.SHA
import Data.Time.Clock

setupTrigger::Connection->IO Int64
setupTrigger conn = do
  withTransaction conn $ execute conn
    "drop trigger if exists quarry on raw_transaction;\n\
    \create or replace function quarry() returns trigger language plpgsql as $$\
    \begin\n\
    \ perform pg_notify('quarry', NULL);\n\
    \ return null;\n\
    \end\n\
    \$$;\n\
    \create trigger quarry after insert on raw_transaction for each row execute procedure quarry();\n\
    \listen quarry;\n" ()

instance HasSQLDB (ReaderT ConnectionPool (NoLoggingT (ResourceT IO))) where
  getSQLDB = ask

main::IO ()
main = do
  conn <- connectPostgreSQL "host=localhost dbname=eth user=postgres password=api port=5432"
  let sConn = runNoLoggingT $ do
        f <- askLogFunc
        lift $ openSimpleConn f conn
  _ <- setupTrigger conn
  forever $ do
    --_ <- getNotification conn
    Notification _ notifChannel notifData <- getNotification conn
    putStr $ "Trigger on " ++ (unpack notifChannel) ++ " data is: " ++ (unpack notifData) ++ "\n"
    ts <- getCurrentTime
    runResourceT $ runNoLoggingT $ withSqlPool (const sConn) 1 $ \sPool -> flip runReaderT sPool $
      putBlocks [
        Block{
           blockBlockData=
              BlockData {
                blockDataParentHash= SHA 0,
                blockDataUnclesHash=hash$ B.pack [0xc0],
                blockDataCoinbase=
                  prvKey2Address $ fromJust $ makePrvKey
                  0x0000000000000000000000000000000000000000000000000000000000000001,
                blockDataStateRoot = MP.SHAPtr "",
                blockDataTransactionsRoot = MP.emptyTriePtr,
                blockDataReceiptsRoot = MP.emptyTriePtr,
                blockDataLogBloom = B.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],         
                blockDataDifficulty = 1000000000000,
                blockDataNumber = 1,
                blockDataGasLimit = 3141592,
                blockDataGasUsed = 0,
                blockDataTimestamp = ts,  
                blockDataExtraData = 0,
                blockDataMixHash = SHA 0,
                blockDataNonce = 5
                },
           blockReceiptTransactions=[],
           blockBlockUncles=[]
           }]

