module Main where

--import ChatBot.Impl

main :: IO ()
main = print "WIP"
{-

withLogHandle :: Config.AppConfig -> System.IO.Handle -> (Logger.Handle IO -> IO ()) -> IO ()
withLogHandle appCfg h f = do
   let cfg = Config.cfgLogger appCfg
   loggerImplHandle <- Logger.Impl.makeSimpleHandle h cfg
   Logger.Impl.withHandle loggerImplHandle

makeBotHandleTEST :: Config.AppConfig -> Logger.Handle IO -> IO State -> ((State -> State) -> IO ()) -> IO (ChatBot.Handle IO)
makeBotHandleTEST appCfg h = let
   cfg = Config.cfgChatBot appCfg

-- getUserState :: 
getUserState :: IO (IORef ChatBot.State)
getUserState 
-}