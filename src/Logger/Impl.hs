-- | Logger Implementation
module Logger.Impl
   ( makeSimpleConfig
   , withHandle
   ) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified System.IO (Handle)

import qualified Logger

-- | There is no way to construct Config manually
data Config m = Config
   { cfgLogMessageImpl :: Text -> m ()
   , cfgMinLogLevel    :: Logger.LogLevel
   }

-- | Config smart constructor, to keep things simple
makeSimpleConfig :: System.IO.Handle -> Logger.LogLevel -> Config IO
makeSimpleConfig handle level = Config logger level
   where
      logger = TIO.hPutStrLn handle

withHandle :: Monad m => Config m -> (Logger.Handle m -> m ()) -> m ()
withHandle cfg f = f $ Logger.Handle (logWith cfg)

logWith :: Monad m => Config m -> Logger.LogLevel -> Text -> m ()
logWith (Config logger minLevel) level msg
   | level >= minLevel = logger msg
   | otherwise         = return ()   