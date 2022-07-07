-- | Here defined Logger implementation.
--    Consider this module as internal.

-- to-do
--    ADD time
--    split config saving into separate module for less coupling.
module Logger.Impl
   ( Handle (..) -- only type
   , Config (..)
   , withHandle
   , logFile
   ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

import Lib ((.<))
import qualified Logger

data Config = Config
   { cfgMinLogLevel :: Logger.LogLevel
   } deriving (Generic, Show)

instance ToJSON Config where
   toEncoding = genericToEncoding defaultOptions

instance FromJSON Config

data Handle m = Handle
   { hLogMessageImpl :: Text -> m ()
   , hConfig         :: Config
   }

logFile :: FilePath
logFile = "log.txt"

-- | This function provides low-level logger handle for application.
withHandle :: Monad m => Handle m -> (Logger.Handle m -> m a) -> m a
withHandle h f = f $ Logger.Handle (logWith h)

logWith :: Monad m => Handle m -> Logger.LogLevel -> Text -> m ()
logWith (Handle logger (Config minLevel)) level msg
   | level >= minLevel = logger $ formateLogMessage level msg
   | otherwise         = return ()

-- ADD time fixation
formateLogMessage :: Logger.LogLevel -> Text -> Text
formateLogMessage level msg = "[" .< level <> "] " <> msg