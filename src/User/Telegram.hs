-- | Module defines DB instance for Telegram users and constructors for DataBase types.
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
module User.Telegram 
   ( TelegramDB
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

import Data.Map.Strict (Map)
import Data.IORef (IORef)
import qualified Data.Map.Strict as Map

import User.DataBase (DB(..))

data TelegramDB

data UserState = UserState
   { repCount :: Int
   } deriving (Show)

instance DB TelegramDB where
   data UserId TelegramDB = UserIdTg Int
      deriving (Show, Eq, Ord)
   data User TelegramDB = UserTg
      { getUserState :: IORef UserState}
   data DataBase TelegramDB = DataBaseTg
      {getDataBase :: Map (UserId TelegramDB) (User TelegramDB)}
   
   insertWithDB f uId user db = DataBaseTg $ Map.insertWith f uId user (getDataBase db)
   deleteUserDB uId db = DataBaseTg $ Map.delete uId (getDataBase db)
   lookupDB     uId db = Map.lookup uId (getDataBase db)
   memberDB     uId db = Map.member uId (getDataBase db)

instance Show (DataBase TelegramDB) where
   show = const "Telegram Map"

saveDataBase :: DataBase TelegramDB -> IO ()
saveDataBase = undefined

loadDataBase :: IO (DataBase TelegramDB)
loadDataBase = undefined

fromListDataBase :: [(UserId TelegramDB, User TelegramDB)] -> DataBase TelegramDB
fromListDataBase = DataBaseTg . Map.fromList 