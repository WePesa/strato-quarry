module Debug where

import Blockchain.Quarry.Flags
import Control.Monad
import Control.Monad.IO.Class

debugPrint :: (MonadIO m) => String -> m ()
debugPrint s = when flags_debug $ liftIO $ putStr s
