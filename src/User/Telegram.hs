-- | Module defines DB instance for Telegram users and constructors of 

{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
module User.Telegram where

import Data.Map.Strict (Map)
import Data.IORef (IORef)
import qualified Data.Map.Strict as Map

import User.DataBase (DB(..))

data TelegramDB

data UserStateTg = UserStateTg
   { repCount :: Int
   } deriving (Show)

instance DB TelegramDB where
   data UserId TelegramDB = UserIdTg Int
      deriving (Show, Eq, Ord)
   data User TelegramDB = UserTg
      { userState :: IORef UserStateTg
      }
   data DataBase TelegramDB = DataBaseTg
      {getDataBase :: Map (UserId TelegramDB) (User TelegramDB)}
   insertWithDB f uId user db = DataBaseTg $ Map.insertWith f uId user (getDataBase db)
   deleteUserDB uId db = DataBaseTg $ Map.delete uId (getDataBase db)
   lookupDB     uId db = Map.lookup uId (getDataBase db)
   memberDB     uId db = Map.member uId (getDataBase db)

instance Show (DataBase TelegramDB) where
   show = const "Telegram Map"