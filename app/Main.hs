-- To-Do
--    Rework imports
--    Work on start up logic
--    Logger imports dont seems accurate
module Main where

import System.IO (withFile, IOMode(..))

import Config (AppConfig, defaultLoggerConfig, initAppConfig)
import ChatBot.Init (createBotHandle)
import Logger.Init (logFile)
import qualified Config
import qualified FrontEnd.Console as Console
import qualified Logger.Init as Logger
import qualified Logger as LowLogger

main :: IO ()
main = do
   appCfg <- initAppConfiguration
   withLoggerHandle appCfg logFile $ \hLogger -> do
      hBot <- createBotHandle (Config.cfgChatBot appCfg) hLogger
      Console.run (Console.Handle hBot)
      
   
initAppConfiguration :: IO AppConfig
initAppConfiguration = do
   withFile "logStart.txt" AppendMode $ \startUpLogFileHandle -> do
      let startUpLoggerHandle = Logger.createHandleIO startUpLogFileHandle defaultLoggerConfig
      Logger.withHandle startUpLoggerHandle initAppConfig

withLoggerHandle :: AppConfig 
                 -> FilePath                    -- ^ Log file
                 -> (LowLogger.Handle IO -> IO ())
                 -> IO ()
withLoggerHandle appCfg path f =
   withFile path AppendMode $ \hLogFile -> do
      let loggerHandle = Logger.createHandleIO hLogFile (Config.cfgLogger appCfg)
      Logger.withHandle loggerHandle f