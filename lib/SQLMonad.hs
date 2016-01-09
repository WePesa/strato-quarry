{-# LANGUAGE FlexibleInstances #-}
module SQLMonad where

import Blockchain.DB.SQLDB
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.ByteString.Char8 (pack)
import Database.Persist.Postgresql hiding (Connection)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Notification
import Database.PostgreSQL.Simple.Types

data SQLConns = SQLConns {
  simpleConn :: Connection,
  persistPool :: ConnectionPool
  }

type ConnT = ReaderT SQLConns (NoLoggingT IO)

instance HasSQLDB (ResourceT ConnT) where
  getSQLDB = persistPool <$> lift ask

withConnInfo :: ConnectInfo -> ConnT a -> IO a
withConnInfo ci cx =
  runNoLoggingT $ runResourceT $ do
    (_, sConn) <- allocate (connect ci) close
    lift $ withSqlPool (\logf -> openSimpleConn logf sConn) 1 $ \pPool ->
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
