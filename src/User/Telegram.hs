-- | Module defines DB instance for Telegram users and constructors for DataBase types.
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- to-do
-- Rename TelegramUser to TelegramUserHandle
module User.Telegram where
{-   ( TelegramDB
   , UserState(..)
   -- ^ New data
   , UserId(UserIdTg)
   , User(UserTg)
   , getUserState
   -- ^ Smart-Constructors for new, but "closed" data
   , saveDataBase
   , loadDataBase
   , fromListDataBase
   -- ^ Initialization/saving data base functions
   ) where
-}
import Control.Monad.IO.Class (MonadIO(), liftIO)
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.Map.Strict as Map

import User.Class (BotUser(..))
import qualified ChatBot (State)
import qualified Logger

data TelegramUser m = TelegramUser
   { mapName :: Text
   , dbLogger :: Logger.Handle m
   }

data UserState = UserState
   { repCount :: Int
   } deriving (Show)

instance MonadIO m => BotUser (TelegramUser m) m Int where
   data UserId (TelegramUser m) = UserIdTg Int
      deriving (Show, Eq, Ord)
   data User (TelegramUser m) = UserTg
      { getUserState :: IORef ChatBot.State}
   data UsersMap (TelegramUser m) = DataBaseTg
      {getDataBase :: Map (UserId (TelegramUser m)) (User (TelegramUser m))}
   
   getUsersMapName = return . mapName
   getLoggerHandle = dbLogger
   
   newUserId = return . UserIdTg
   defaultUser state = do
      stateRef <- liftIO $ newIORef state
      return $ UserTg stateRef
   
   getBotState = liftIO . readIORef . getUserState
   modifyBotState user f = liftIO $ modifyIORef' (getUserState user) f 
   
   fromListUM = return . DataBaseTg . Map.fromList
   insertWithUM f uId user m = return . DataBaseTg $ Map.insertWith f uId user (getDataBase m)
   deleteUserUM uId m = return . DataBaseTg $ Map.delete uId (getDataBase m)
   lookupUM     uId m = return $ Map.lookup uId (getDataBase m)
   memberUM     uId m = return $ Map.member uId (getDataBase m)