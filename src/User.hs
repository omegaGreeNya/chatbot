-- | This module provides functions for working with users based @BotUser@ class User.Class module.
-- Currently implemented platforms: Telegram API users (module User.Telegram).
module User
   ( module X
   , addUser
   , deleteUser
   , lookupUser
   ) where

-- We HIDE UsersMap manipulation functions of BotUser class.
import User.Class as X
   (BotUser
      ( UserId, User, UsersMap        -- Associated Data Types
      , newUserId, defaultUser        -- UserId and User smart-constructors
      , getBotState, modifyBotState   -- ChatBot.State methods for UsersMap
      )
   )

import User.Class (BotUser(..))
import Lib ((.<~))
import Logger (logDebug, logWarning)


-- | Adds @User@ into @UsersMap@. If user already there, logs error and returns same map.
addUser :: (BotUser user m t)
        => user
        -> UserId user       -- ^ Id of new user to add
        -> User user         -- ^ New user to add
        -> UsersMap user     -- ^ UsersMap to update
        -> m (UsersMap user) -- ^ Updated Users Map
addUser u userId user usersMap = do
   mapName <- getUsersMapName u
   presented <- memberUM userId usersMap
   if presented
   then do
      logWarning (getLoggerHandle u) $
         "Tried to add User with" .<~ userId <> " but this id was already in \"" <> mapName <> "\"."
      return usersMap
   else do
      logDebug (getLoggerHandle u) $ "Added " .<~ userId <> " into \"" <> mapName <> "\"."
      insertWithUM (const id) userId user usersMap

-- | Delets @User@ from @UsersMap@. If @UsersMap@ doesn't contain provided @UserId@ logs warning.
deleteUser :: (BotUser user m t)
           => user
           -> UserId user       -- ^ Id of new user to add
           -> UsersMap user     -- ^ Telegram users Map to delete from
           -> m (UsersMap user) -- ^ Updated Users Map
deleteUser u userId usersMap = do
   mapName <- getUsersMapName u
   presented <- memberUM userId usersMap
   if (not presented)
   then do
      logWarning (getLoggerHandle u) $
         "Tried to delete User with" .<~ userId <> " but \"" <> mapName <> "\" doesn't contain this id."
      return usersMap
   else do
      logDebug (getLoggerHandle u) $ "Deleted " .<~ userId <> " from \"" <> mapName <> "\"."
      deleteUserUM userId usersMap

-- | Tries to find @User@ with @UserId@ inside @UsersMap@. Logs debug info of search result.
lookupUser :: (BotUser user m t)
           => user
           -> UserId user           -- ^ Id of new user to add
           -> UsersMap user         -- ^ Telegram users Map to delete from
           -> m (Maybe (User user)) -- ^ Just @User@ if @UsersMap@ contains user with @UserId@. Nothing otherwise.
lookupUser u userId usersMap = do
   mapName <- getUsersMapName u
   mUser <- lookupUM userId usersMap
   case mUser of
      Just user -> do
         logDebug (getLoggerHandle u) $
            "Finded User with" .<~ userId <> " in \"" <> mapName <> "\"."
         return $ Just user
      _         -> do
         logDebug (getLoggerHandle u) $
            "Tried to find User with" .<~ userId <> " in \"" <> mapName <> "\", but found none."
         return Nothing
