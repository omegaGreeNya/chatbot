-- | This is core module of logic based on classes
module App where

import FrontEnd.Class (Front(..))
import Message (Message(..))
import User (BotUser(..), addUser, lookupUser)

import qualified ChatBot
import qualified Logger

data Handle fHandle uHandle m = Handle
   { getFrontHandle     :: fHandle
   , getUserHandle      :: uHandle
   , hLogger            :: Logger.Handle m
   , constructBotHandle :: User uHandle -> m (ChatBot.Handle m)
   , saveUsersMap       :: UsersMap uHandle -> m ()
   }

-- | Actual application loop.
-- Works with provided handle and users map.
runFront :: ( Front fHandle fMessage uIdType m
            , BotUser uHandle uIdType m)
         => Handle fHandle uHandle m
         -> UsersMap uHandle
         -> m ()
runFront h usersMap = do
   messages  <- getMessages (getFrontHandle h)
   (responses, usersMap') <- getResponses h usersMap messages
   _ <- saveUsersMap h usersMap'
   _ <- mapM_ (uncurry $ sendResponse (getFrontHandle h)) responses
   runFront h usersMap'

-- | Wraps calls to ChatBot Logic
getResponses :: ( Front fHandle fMessage uIdType m
                , BotUser uHandle uIdType m)
             => Handle fHandle uHandle m
             -> UsersMap uHandle
             -> [(uIdType, ChatBot.Event fMessage)]
             -> m ([(uIdType, ChatBot.Response fMessage)]
                  , UsersMap uHandle)
getResponses _ usersMap [] =
   return ([], usersMap)
getResponses h usersMap ((idRaw, event):messages) = do
   (responses, usersMap') <- processMessage h usersMap idRaw event
   (results, usersMap'') <- getResponses h usersMap' messages
   return (responses <> results
          , usersMap'')

-- | Transforms single message to list of response (by bot logic)
processMessage :: ( Message msg
                  , BotUser uHandle uIdType m)
               => Handle fHandle uHandle m
               -> UsersMap uHandle
               -> uIdType
               -> ChatBot.Event msg
               -> m ([(uIdType, ChatBot.Response msg)]
                    , UsersMap uHandle)
processMessage h usersMap idRaw event = do
   uId <- newUserId idRaw
   (user, usersMap') <- findOrCreateUser (getUserHandle h) uId usersMap
   botHandle <- constructBotHandle h user
   responses <- ChatBot.respond botHandle event
   return ( map (\response -> (idRaw, response)) responses
          , usersMap')

-- | Tries to find user inside map
-- if user not found creates new one and adds it to the map
findOrCreateUser :: (BotUser uHandle uIdType m)
                 => uHandle
                 -> UserId uHandle
                 -> UsersMap uHandle
                 -> m (User uHandle, UsersMap uHandle)
findOrCreateUser uHandle uId usersMap = do
   mUser <- lookupUser uHandle uId usersMap
   case mUser of
      Just user -> return (user, usersMap)
      Nothing   -> do
         user <- newUser uHandle
         usersMap' <- addUser uHandle uId user usersMap
         return (user, usersMap')
