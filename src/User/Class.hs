-- | This module provides abstraction for definig relations with UsersMap, UserId and User - class @BotUser@.
-- This is internal module, use only to add new instances. For working @BotUser@ class take User module.
--
-- This module heavely exploits phantom types for restricting adding @User@ into unsuit @UsersMap@ on the type level.
-- More info about Phantom types: https://wiki.haskell.org/Phantom_type
--                                https://kowainik.github.io/posts/haskell-mini-patterns#phantom-type-parameters
--
-- See User.Telegram for example of instance of DB class.
-- See FrontEnd for example of using @BotUser@ class functional.
-- To-Do 
-- Add default methods, if it's reasonable.
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
module User.Class
   ( BotUser(..)
   ) where

import Data.Kind (Type)
import Data.Text (Text)

import qualified ChatBot (State)
import qualified Logger

-- | This class unifies working with different user types, from different front-ends.
-- Type variable t is id type (for example, one API may have Strings ids, another Int ids)
-- @UserId@ is the key, and @User@ is Value.
-- If you want to store mutable Values, make such @User@ that contains reference for mutable variable.
-- And same as @User@, @UsersMap@ also can contain referece for mutable map.
-- "user" is somehow mimics handle-pattern, and it acts as phantom type for associated data.
-- Defining instance of this class provides access to working with functions from User module.
class ( Show (UserId user)
      , Ord  (UserId user)
      , Monad m)
   => BotUser user m t | user -> t where
   data UserId user :: Type
   -- ^ Key for UsersMap. Show, Eq, Ord must be defined for @UserId@.
   data User user     :: Type
   -- ^ Value for UsersMap, should containt all info to construct ChatBot.State.
   data UsersMap user :: Type
   -- ^ Users container. @UserId@ would be used as key for @User@.
   -- Also, @UserMap@ may be mutable, if you need.
   getUsersMapName  :: user -> m Text
   -- ^ Map name, used for logging.
   getLoggerHandle  :: user -> Logger.Handle m
   -- ^ Loging done automatic by functions from @User@ module (data base related functions).
   -- But, if you reaally need, you can add loging into methods below.
   newUserId        :: t -> m (UserId user)
   -- ^ Smart-Constructor for @UserId@ type. t represents type of id that API uses.
   defaultUser      :: ChatBot.State -> m (User user)
   -- ^ Creating new user with provided bot state for him
   getBotState      :: User user -> m (ChatBot.State)
   -- ^ Bot settings acessor for @User@ type
   modifyBotStateUM   :: (ChatBot.State -> ChatBot.State) -> (UserId user) -> (UsersMap user) -> m ()
   -- ^ Function for updating @ChatBot.State@ for user with @UserID@. 
   -- YOU DON'T NEED TO CARE DOES @UsersMap@ CONTAINS @UserId@ OR NOT, JUST "return ()" IF USERID WASN'T FOUND.
   -- This is inner function, and functions from User module will wrap modifyBotStateUM in a proper way.
   fromListUM       :: [(UserId user, User user)] -> m (UsersMap user)
   -- ^ Creats Data Base from pairs of id and vals. Should return empty UsersMap on empty List.
   insertWithUM     :: (User user -> User user -> User user) -> UserId user -> User user -> UsersMap user -> m (UsersMap user)
   -- ^ This function would be used for adding users into @UsersMap@
   deleteUserUM     :: UserId user -> UsersMap user -> m (UsersMap user)
   -- ^ Delets user if he's presented, othewise returns same DB.
   lookupUM         :: UserId user -> UsersMap user -> m (Maybe (User user))
   -- ^ This function would be used for searching users in @UsersMap@.
   -- Should return @Just@ @User@, if user presents, Nothing otherwise
   memberUM         :: UserId user -> UsersMap user -> m (Bool)
   -- ^ Function should retun True if @UsersMap@ contains @User@