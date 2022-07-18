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
   ( DB(..)
   ) where

import Data.Kind (Type)

-- | This class unifies working with different user types, from different front-end or data bases.
-- UserId is the key, and User is Value.
-- If you want to store mutable Values, make such User that contains reference for mutable variable.
-- Class designed to be used with phantom db, since we don't need any data inside db. But this is not a requirement.
-- Defining instance of this class provides access to working with functions from User module.
-- After providing instance, you will only need do make initializing @DataBase@ functional to start working with @DataBase@.
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