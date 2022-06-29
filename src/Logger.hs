-- | Logger interface
module Logger
   ( LogLevel (..)
   , Handle (..)
   , logDebug
   , logInfo
   , logWarning
   , logError
   ) where

import Data.Aeson hiding (Error)
import Data.Text (Text)
import GHC.Generics

data Handle m = Handle
   { hLogMessage :: LogLevel -> Text -> m ()
   }

data LogLevel
   = DEBUG
   | INFO
   | WARN
   | ERROR
   deriving (Generic, Show, Eq, Ord)

instance ToJSON LogLevel where
   toEncoding = genericToEncoding defaultOptions

instance FromJSON LogLevel

logDebug, logInfo, logWarning, logError :: Handle m -> Text -> m ()
logDebug h = hLogMessage h DEBUG
logInfo h = hLogMessage h INFO
logWarning h = hLogMessage h WARN
logError h = hLogMessage h ERROR
