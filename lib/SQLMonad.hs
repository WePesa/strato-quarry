{-# LANGUAGE FlexibleInstances #-}
module SQLMonad where

import Blockchain.DB.SQLDB
import Blockchain.EthConf
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
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

type ConnT = ReaderT SQLConns (NoLoggingT IO)

instance HasSQLDB (ResourceT ConnT) where
  getSQLDB = persistPool <$> lift ask

runConnT :: ConnT a -> IO a
runConnT cx =
  let cs = postgreSQLConnectionString $ sqlConfig ethConf
  in runNoLoggingT $ runResourceT $ do
    (_, sConn) <- allocate (connectPostgreSQL cs) close
    -- 2 is important here so long as the addBlock hack is being used
    -- in BlockConstruction.hs
    lift $ withPostgresqlPool cs 2 $ \pPool ->
      runReaderT cx 
      SQLConns {
        simpleConn = sConn,
        persistPool = pPool
        }

asSimpleTransaction :: [String] -> ConnT ()
asSimpleTransaction ss = do
  sConn <- simpleConn <$> ask
  let as = mapM_ (execute_ sConn . Query . pack) ss  
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
