-- | This module provides abstraction for definig relations with DataBase, UserId and User,
-- and functions for working with database. Base on class DB.
--
-- This module heavely exploits phantom types for restricting adding @User@ into unsuit @DataBase@ on the type level.
-- More info about Phantom types: https://wiki.haskell.org/Phantom_type
--                                https://kowainik.github.io/posts/haskell-mini-patterns#phantom-type-parameters
--
-- See User.Telegram for example of instance of DB class
-- See FrontEnd.Telegram for example of using this module
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module User.DataBase
   ( Handle (..)
   , DB(..)
   , addUser
   , deleteUser
   , lookupUser
   ) where

import Data.Kind (Type)

import Lib ((.<~))
import Logger (logDebug, logWarning)
import qualified Logger

data Handle m = Handle
   { hLogger :: Logger.Handle m
   }

-- | This class unifies working with different user types, from different front-end or data bases.
-- UserId is the key, and User is Value.
-- If you want to store mutable Values, make such User that contains reference for mutable variable.
-- Defining instance of this class provides acces to working with functions from this module.
-- Designed to be used with phantom db, since we don't need any data inside db. But this is not a requirement.
class ( Show (UserId db)
      , Ord  (UserId db)
      , Show (DataBase db))
   => DB db where
   data UserId db   :: Type
   -- ^ Key for DataBase. Show, Eq, Ord must be defined for @UserId@ db
   data User db     :: Type
   -- ^ Value for DataBase
   data DataBase db :: Type 
   -- ^ Data Base.
   -- Must be instance of Show. Show instance used for logging, so i recomend something simple like (constant "db name").
   insertWithDB     :: (User db -> User db -> User db) -> UserId db -> User db -> DataBase db -> DataBase db
   -- ^ This function would be used for adding users into @DataBase@
   deleteUserDB     :: UserId db -> DataBase db -> DataBase db
   -- ^ Delets user if he's presented, othewise returns same DB.
   lookupDB         :: UserId db -> DataBase db -> Maybe (User db)
   -- ^ This function would be used for searching users in @DataBase@.
   -- Should return @Just@ @User@, if user presents, Nothing otherwise
   memberDB         :: UserId db -> DataBase db -> Bool
   -- ^ Function should retun True if @DataBase@ contains @User@

-- | Adds User into DataBase. If user already there, logs error and returns same map.
addUser :: (Monad m, DB db)
        => Handle m
        -> UserId db       -- ^ Id of new user to add
        -> User db         -- ^ New user to add
        -> DataBase db     -- ^ DataBase to update
        -> m (DataBase db) -- ^ Updated Users Map
addUser h newUserId newUser dataBase =
   if (memberDB newUserId dataBase)
   then do
      logWarning (hLogger h) $
         "Tried to add User with" .<~ newUserId <> " but it was already in " .<~ dataBase <> "."
      return dataBase
   else do
      logDebug (hLogger h) $ "Added " .<~ newUserId <> " into " .<~ dataBase <> "."
      return $ insertWithDB (const id) newUserId newUser dataBase

deleteUser :: (Monad m, DB db)
           => Handle m
           -> UserId db       -- ^ Id of new user to add
           -> DataBase db     -- ^ Telegram users Map to delete from
           -> m (DataBase db) -- ^ Updated Users Map
deleteUser h userId dataBase =
   if (not $ memberDB userId dataBase)
   then do
      logWarning (hLogger h) $
         "Tried to delete User with" .<~ userId <> " but it was already in " .<~ dataBase <> "."
      return dataBase
   else do
      logDebug (hLogger h) $ "Deleted " .<~ userId <> " from " .<~ dataBase <> "."
      return $ deleteUserDB userId dataBase

-- | Tries to find @User@ with @UserId@ inside @DataBase@. Logs debug info of search result
lookupUser :: (Monad m, DB db)
           => Handle m
           -> UserId db           -- ^ Id of new user to add
           -> DataBase db         -- ^ Telegram users Map to delete from
           -> m (Maybe (User db)) -- ^ Just @User@ if @DataBase@ contains user with @UserId@. Nothing otherwise.
lookupUser h userId dataBase =
   case lookupDB userId dataBase of
   Just user -> do
      logDebug (hLogger h) $
         "Finded User with" .<~ userId <> " in " .<~ dataBase <> "."
      return $ Just user
   _         -> do
      logDebug (hLogger h) $
         "Tried to find User with" .<~ userId <> " in " .<~ dataBase <> ", but found none."
      return Nothing