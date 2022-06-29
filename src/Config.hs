-- | Config initialization


-- TO-DO
-- 
-- add define default texts
module Config where

import Data.Aeson
import GHC.Generics
import System.IO
import qualified Data.ByteString.Lazy as B
import qualified ChatBot
import qualified Logger.Impl
import qualified Logger

data AppConfig = AppConfig
   { cfgChatBot :: ChatBot.Config
   , cfgLogger  :: Logger.Impl.Config
   } deriving (Generic, Show)

instance ToJSON AppConfig where
   toEncoding = genericToEncoding defaultOptions

instance FromJSON AppConfig

initConfig :: Maybe Logger.LogLevel -> IO (Maybe AppConfig)
initConfig = undefined {-do
   mConfig <- try $ getConfig
   case mConfig of
      Nothing -> Logger.logWarning ""
-}
getConfig :: IO (Maybe AppConfig)
getConfig = withFile "config.json" ReadMode
   ( fmap decode
   . B.hGetContents
   )

-- | If there is no defined config/it's malformed, we will write pre-defined one.
defaultConfig :: AppConfig 
defaultConfig = AppConfig defaultChatBotConfig defaultLoggerConfig

defaultChatBotConfig :: ChatBot.Config
defaultChatBotConfig = ChatBot.Config
   "/help text here"
   "/repeat text here"
   5
   1

defaultLoggerConfig :: Logger.Impl.Config
defaultLoggerConfig = Logger.Impl.Config Logger.INFO

