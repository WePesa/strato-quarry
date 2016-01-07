module SQLMonad where

import Blockchain.DB.SQLDB
import Control.Monad.Trans.Logger
import Control.Monad.Trans.Resource
import Data.ByteString.Char8 (pack)
import Database.Persist.SQL
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

data SQLConns = SQLConns {
  simpleConn :: Connection
  persistPool :: ConnectionPool
  }

type ConnT = ReaderT SQLConns (ResourceT (LoggerT IO))

instance HasSQLDB ConnT where
  getSQLDB = persistPool <$> ask

withConnInfo :: ConnectInfo -> ConnT a -> IO a
withConnInfo ci cx =
  runStdoutLoggerT $ runResourceT $ do
    (_, sConn) <- allocate (connect ci) close
    lift $ withSqlPool (\log -> openSimpleConn log sConn) 1 $ \pPool ->
      runReaderT
      SQLConns {
        simpleConn = sConn,
        persistPool = pPool
        }
      cx

asSimpleTransaction :: [String] -> ConnT ()
asSimpleTransaction ss = do
  sConn <- simpleConn <$> ask
  let as = map (execute _ sConn . Query . pack) ss  
  _ <- liftIO $ withTransaction sConn as
  return ()

asPersistTransaction :: SqlPersistT ConnT a -> ConnT a
asPersistTransaction q = do
  pPool <- persistPool <$> ask
  runSqlPool q pPool
