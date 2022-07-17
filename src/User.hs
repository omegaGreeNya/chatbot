-- | This module provides user abstraction,
-- and simpliest User data-base with methods.
--
-- This module heavely exploits phantom types for restricting adding @User@ into unsuit @Map@ on the type level.
-- More info about Phantom types: https://wiki.haskell.org/Phantom_type
--                                https://kowainik.github.io/posts/haskell-mini-patterns#phantom-type-parameters
--
-- Notice that Module do not export Data Constructors for UserId/User/UsersMap. Only smart constructors and special acessors for them.
{-# LANGUAGE FlexibleInstances #-} -- for Show instance with Phatom type
{-# LANGUAGE FlexibleContexts  #-} -- for Type Signature context with Phantom type
-- To-Do
-- It's possible to rework for mutable maps, but for simple dead-birth echo bot should be sufficient.
module User 
   ( Handle (..)
   , Telegram
   , UserId
   , getTelegramUserId
   , User
   , userId
   , userState
   , UserState (..)
   , UsersMap
   , createTelegramUser
   , addUser
   , lookupUser
   ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict (Map)
import Data.IORef (IORef, newIORef)
import qualified Data.Map.Strict as Map (insertWith, lookup, member)

import Lib ((.<~))
import Logger (logInfo, logDebug, logWarning)
import qualified Logger

-- | User manipulation handle.
data Handle m = Handle
   { hLogger :: Logger.Handle m
   -- ^ See Loger module
   , hConfig :: Config
   -- ^ Settings
   }

-- | Various settings for working with users.
data Config = Config
   { cfgRepetitionCount :: Int
   -- ^ Default, would be assigned to new users.
   } deriving (Show)

-- | Phantom Type Constructor.
data Telegram
-- DO NOT FORGET TO ADD SHOW INSTANCE FOR NEW USERSMAP OF ADDED PHANTOM

-- | User id parametrized by phantom Type Constructor.
-- As protection to this design decision, i should say that some api may have literals ids.
data UserId db
   = TelegramUserId Int
   -- ^ Telegram uses ints as id , max is 2^52, Int is enough
   | VKUserId String
   -- ^ JUST SHOWCASE. Not used. Defined to mute reduntant-pattern-match warnings.
   deriving (Show, Eq, Ord)

-- | Returns user identifier, if UserID malformed returns Nothing 
getTelegramUserId :: UserId Telegram -> Maybe Int
getTelegramUserId (TelegramUserId n) = Just n
getTelegramUserId _                  = Nothing

-- | User id and reference for his state. Phantom Type Constructor represents platform user from.
data User db = User
   { userId    :: UserId db
   -- ^ It's ID
   , userState :: IORef UserState
   -- ^ Reference to payload (all that we need to know about user)
   }
-- WARN: DO NOT ADD INFORMATION INTO USER. IT'S DESIGNED TO CONTAIN ONLY IT'S ID AND SOME REFERENCES.
-- Im not even sure, what you will need to change it at all.

-- | Actual User data. @User@ contains only reference on @UserState@
data UserState = UserState
   { userRepetitionCount :: Int
   -- ^ How much echoed message user should receive
   } deriving (Show)

-- | Map of users. Map parametrized by same platform type as users (and users id) it contains.
newtype UsersMap db = UsersMap
   { getUsersMap :: Map (UserId db) (User db)
   }

instance Show (UsersMap Telegram) where
   show = const "Telegram Map"

-- | Creates Telegram User. It's strongly recomended to add user into map with addUser afterwards.
createTelegramUser :: MonadIO m
                   => Handle m
                   -> Int                                -- ^ Telegram user id
                   -> m (UserId Telegram, User Telegram) -- ^ Signed user id, and his setting container.
createTelegramUser h uId = do
   let newState = UserState (cfgRepetitionCount . hConfig $ h)
       newId = TelegramUserId uId
   newRef <- liftIO $ newIORef newState
   let newUser = User newId newRef
   logInfo (hLogger h) $
      "Adding new user with " .<~ newId <> "."
   return (newId, newUser)

-- | Adds Telegram User into Map. If user is already presents, logs error and returns same map.
addUser :: (Monad m, Show (UsersMap db))
        => Handle m
        -> UsersMap db     -- ^ Telegram users Map to update
        -> User db         -- ^ New user to add
        -> m (UsersMap db) -- ^ Updated Users Map
addUser h uMap' user@(User uId _) = do
   let uMap = getUsersMap uMap'
   logInfo (hLogger h) $ "Added " .<~ uId <> " into " .<~ uMap'
   when (Map.member uId uMap) $
      logWarning (hLogger h) $
         "Tried to add User with" .<~ uId <> " but it was already there. Map contains references! Not actual data!"
         <> "If you want to update user State, make a lookup and modify State by it's Ref."
   return . UsersMap $ 
      Map.insertWith (const id) uId user uMap

-- | Looks up user in Telegram Map.
lookupUser :: (Monad m, Show (UsersMap db))
           => Handle m
           -> UsersMap db         -- ^ Telegram users Map to update
           -> UserId db           -- ^ Telegram user id to lookup
           -> m (Maybe (User db)) -- ^ Updated Users Map
lookupUser h uMap' uId = do
   let uMap = getUsersMap uMap'
   logDebug (hLogger h) $ "Searched " .<~ uId <> " in " .<~ uMap'
   return $ 
      Map.lookup uId uMap
