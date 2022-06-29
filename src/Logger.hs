-- | Logger interface
module Logger
   ( LogLevel (..)
   , Handle (..)
   , logDebug
   , logInfo
   , logWarning
   , logError
   ) where

import Data.Text (Text)

data Handle m = Handle
   { hLogMessage :: LogLevel -> Text -> m ()
   }

data LogLevel
   = Debug
   | Info
   | Warning
   | Error deriving (Show, Eq, Ord)

logDebug, logInfo, logWarning, logError :: Handle m -> Text -> m ()
logDebug h = hLogMessage h Debug
logInfo h = hLogMessage h Info
logWarning h = hLogMessage h Warning
logError h = hLogMessage h Error
