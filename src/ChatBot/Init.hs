-- | Bot initilization and control interface.

-- Add user state map
module ChatBot.Init
   ( createBotHandle
   --, runBot
   --, killBot
   ) where

import Data.IORef (newIORef, readIORef, modifyIORef)

import qualified ChatBot
import qualified Logger

-- Map userID State
-- MonadIO m =>
createBotHandle :: ChatBot.Config -> Logger.Handle IO -> IO (ChatBot.Handle IO)
createBotHandle cfg hLogger = do
   (getState, modState) <- createState cfg
   return $ ChatBot.Handle cfg hLogger getState modState

createState :: ChatBot.Config -> IO (IO ChatBot.State, (ChatBot.State -> ChatBot.State) -> IO ())
createState cfg = do
   ref <- newIORef . ChatBot.State $ ChatBot.cfgDefaultRepeatCount cfg
   return (readIORef ref, \f -> modifyIORef ref f)