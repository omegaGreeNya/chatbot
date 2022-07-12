-- To-Do
--    Rework imports
--    Work on start up logic
module Main where

import System.IO (withFile, IOMode(..))

import Config (AppConfig, defaultLoggerConfig, initAppConfig)
import ChatBot.Init (createBotHandle)
import Logger.Init (logFile)
import qualified Config
import qualified FrontEnd.Console as Console
import qualified Logger.Init as Logger

main :: IO ()
main = do
   appCfg <- initAppConfiguration
   withFile logFile AppendMode $ \hLogFile -> do
      let hLogger = Logger.createHandleIO hLogFile (Config.cfgLogger appCfg)
      Logger.withHandle hLogger $ \hLowLogger -> do
         hBot <- createBotHandle (Config.cfgChatBot appCfg) hLowLogger
         Console.run (Console.Handle hBot)
      
-- move to Config
initAppConfiguration :: IO AppConfig
initAppConfiguration = do
   withFile "logStart.txt" AppendMode $ \startUpLogFileHandle -> do
      let startUpLoggerHandle = Logger.createHandleIO startUpLogFileHandle defaultLoggerConfig
      Logger.withHandle startUpLoggerHandle initAppConfig