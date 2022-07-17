-- | This module provides user abstraction,
-- and simpliest User data-base with methods.
--
-- This module heavely exploits phantom types for restricting adding @User@ into unsuit @Map@ on the type level.
-- More info about Phantom types: https://wiki.haskell.org/Phantom_type
--                                https://kowainik.github.io/posts/haskell-mini-patterns#phantom-type-parameters
--
-- Notice that Module do not export Data Constructors for UserId/User/UsersMap. Only smart constructors and special acessors for them.
--
-- To-Do
-- It should be fine with immutable maps, but for simple dead-birth echo bot should be sufficient.
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
   , addTelegramUser
   , lookupTelegramUser
   ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict (Map)
import Data.IORef (IORef, newIORef)
import qualified Data.Map.Strict as Map (insertWith, lookup, member)

import Lib ((.<~))
import Logger (logInfo, logError, logDebug, logWarning)
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
   { uSRepetitionCount :: Int
   -- ^ How much echoed message user should receive
   } deriving (Show)

-- | Map of users. Map parametrized by same platform type as users (and users id) it contains.
newtype UsersMap db = UsersMap
   { getUsersMap :: Map (UserId db) (User db)
   }

-- | Creates Telegram User. It's strongly recomended to add user into map with add*User afterwards.
createTelegramUser :: MonadIO m
                   => Handle 
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
addTelegramUser :: (Monad m)
                => Handle m
                -> UsersMap Telegram     -- ^ Telegram users Map to update
                -> User Telegram         -- ^ New user to add
                -> m (UsersMap Telegram) -- ^ Updated Users Map
addTelegramUser h uMap' user@(User uId@(TelegramUserId _) _) = do
   let uMap = getUsersMap uMap'
   logInfo (hLogger h) $ "Added " .<~ uId <> " into Telegram Map."
   when (Map.member uId uMap) $
      logWarning (hLogger h) $
         "Tried to add User with" .<~ uId <> " but it was already there. Map contains references! Not actual data!"
         <> "If you want to update user State, make a lookup and modify State be it's Ref."
   return . UsersMap $ 
      Map.insertWith (const id) uId user uMap
addTelegramUser h uMap (User uId _) = do
-- This should never happen, but if happens, we log error and don't add user.
   logError (hLogger h) $
      "Can't add " .<~ uId <> " into Telegram Map. Probably UserId was wrong constucted. User was not added."
   return uMap

-- | Looks up user in Telegram Map.
lookupTelegramUser :: (Monad m)
                   => Handle m
                   -> UsersMap Telegram         -- ^ Telegram users Map to update
                   -> UserId Telegram           -- ^ Telegram user id to lookup
                   -> m (Maybe (User Telegram)) -- ^ Updated Users Map
lookupTelegramUser h uMap' uId@(TelegramUserId _) = do
   let uMap = getUsersMap uMap'
   logDebug (hLogger h) $ "Searched " .<~ uId <> " in Telegram Map."
   return $ 
      Map.lookup uId uMap
lookupTelegramUser h _ uId = do
-- This should never happen, but if happens, we log error and return Nothing.
   logError (hLogger h) $
      "Tried to search " .<~ uId <> " in Telegram Map. Probably UserId was wrong constucted."
   return Nothing

