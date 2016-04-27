{-# LANGUAGE FlexibleInstances #-}
module SQLMonad where

import Blockchain.DB.SQLDB
import Blockchain.EthConf
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.ByteString.Char8 (pack)
import Database.Persist.Postgresql hiding (Connection) 
import Database.PostgreSQL.Simple hiding (postgreSQLConnectionString)
import Database.PostgreSQL.Simple.Notification
import Database.PostgreSQL.Simple.Types

data SQLConns = SQLConns {
  simpleConn :: Connection,
  persistPool :: ConnectionPool
  }

type ConnT = ReaderT SQLConns (NoLoggingT (ResourceT IO))

instance HasSQLDB ConnT where
  getSQLDB = persistPool <$> ask

runConnT :: ConnT a -> IO a
runConnT conn =
  let cs = postgreSQLConnectionString $ sqlConfig ethConf
  in runResourceT $ runNoLoggingT $ do
    (_, sConn) <- allocate (connectPostgreSQL cs) close
    -- 2 is important here so long as the addBlock hack is being used
    -- in BlockConstruction.hs
    withPostgresqlPool cs 2 $ \pPool ->
      runReaderT conn 
      SQLConns {
        simpleConn = sConn,
        persistPool = pPool
        }

asSimpleTransaction :: [String] -> ConnT ()
asSimpleTransaction ss = do
  liftIO $ putStrLn $ "Running simpleConn"
  sConn <- simpleConn <$> ask
  let as = mapM_ (\s -> do {liftIO $ putStrLn $ "Running line " ++ s; (execute_ sConn . Query . pack) s }) ss
  --let as = mapM_ (execute_ sConn . Query . pack) ss  
  liftIO $ putStrLn $ "Running withTransaction"
  _ <- liftIO $ withTransaction sConn as
  return ()

asPersistTransaction :: SqlPersistT ConnT a -> ConnT a
asPersistTransaction q = do
  pPool <- persistPool <$> ask
  runSqlPool q pPool

waitNotification :: ConnT Notification
waitNotification = do
  sConn <- simpleConn <$> ask
  liftIO $ getNotification sConn
