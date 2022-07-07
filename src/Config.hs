-- | Config initialization


-- TO-DO
-- (!!!) Add logic to check that config REALLY MISSING.
-- Test IO actions manually
module Config where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import GHC.Generics

import qualified Data.ByteString.Lazy as B

import Lib

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

configPath :: FilePath
configPath = "config.json"

configBackupPath :: FilePath
configBackupPath = locPath <> "malformed_" <> fileName
   where (locPath, fileName) = splitFileName configPath

-- Returns config from "config.json" if possible, 
--   otherwise returns default config and writes it to config.json
--   Also, if config malformed, saves malformed version
initConfig :: MonadIO m => Logger.Handle m -> m (AppConfig)
initConfig h = do
   eConfig <- readFileLog B.readFile h Nothing configPath
   case eConfig of
      -- File missing/unavaible
      Left _ -> do
         -- (!!!) Add logic to check that config REALLY MISSING.
         Logger.logInfo h "Default config would be used"
         _ <- writeFileLog B.writeFile h (Just $ "Saving default config " .<~ configPath) configPath (encode defaultConfig)
         return defaultConfig
      -- File readed, start decoding it
      Right bsConfig -> decodeConfig h bsConfig


-- Maybe do not replace malformed cfg??
decodeConfig :: MonadIO m => Logger.Handle m -> ByteString -> m (AppConfig)
decodeConfig h bsConfig = case decode bsConfig of
            -- Config file malformed case
            Nothing -> do
               Logger.logWarning h $
                  configPath ~>. " is malformed, malformed version saved as " .<~ configBackupPath
               -- Saving malformed file just in case.
               _ <- writeFileLog B.writeFile h Nothing configBackupPath bsConfig
               -- Replace malformed config file with default configuration
               Logger.logInfo h "Default config would be used"
               _ <- writeFileLog B.writeFile h (Just $ "Saving default config " .<~ configPath) configPath (encode defaultConfig)
               return defaultConfig
            -- Config file is OK, return it
            Just config -> return config

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