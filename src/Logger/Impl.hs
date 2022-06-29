-- | Logger Implementation
module Logger.Impl
   ( Config (..)
   , makeSimpleHandle
   , withHandle
   ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Data.Text.IO as TIO
import qualified System.IO (Handle)

import qualified Logger

data Config = Config
   { cfgMinLogLevel :: Logger.LogLevel
   } deriving (Generic, Show)

instance ToJSON Config where
   toEncoding = genericToEncoding defaultOptions

instance FromJSON Config

-- | There is no way to construct Handle manually
data Handle m = Handle
   { hLogMessageImpl :: Text -> m ()
   , hConfig         :: Config
   }

-- | Handle smart constructor, to keep things simple
makeSimpleHandle :: System.IO.Handle -> Config -> Handle IO
makeSimpleHandle handle cfg = Handle logger cfg
   where
      logger = TIO.hPutStrLn handle

withHandle :: Monad m => Handle m -> (Logger.Handle m -> m ()) -> m ()
withHandle h f = f $ Logger.Handle (logWith h)

logWith :: Monad m => Handle m -> Logger.LogLevel -> Text -> m ()
logWith (Handle logger (Config minLevel)) level msg
   | level >= minLevel = logger msg
   | otherwise         = return ()   