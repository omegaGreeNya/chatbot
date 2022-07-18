-- | This module provides functions for working with DataBase based on UserId and User from User.DataBase module.
-- Currently implemented platforms: Telegram API users States (User.Telegram).
module User
   ( module X
   , Handle(..)
   , addUser
   , deleteUser
   , lookupUser
   ) where

-- We hide inner functions of DB class, and export only Data Types
import User.DataBase as X (DB(UserId, User, DataBase))

import User.DataBase (DB(..))
import Lib ((.<~))
import Logger (logDebug, logWarning)
import qualified Logger

data Handle m = Handle
   { hLogger :: Logger.Handle m
   }

-- | Adds @User@ into @DataBase@. If user already there, logs error and returns same map.
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

-- | Delets @User@ from @DataBase@. If DataBase doesn't contain provided @UserId@ logs warning.
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

-- | Tries to find @User@ with @UserId@ inside @DataBase@. Logs debug info of search result.
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