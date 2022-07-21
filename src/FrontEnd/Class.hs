-- | Module contains class to work with different API by defined methods.
-- See FrontEnd.Telegram for instance example
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
module FrontEnd.Class 
   (Front(..)) where

import Data.Kind (Type)

import Message (Message(..))
import User.Class (BotUser(..))
import qualified ChatBot (Handle, Response, Event)

-- | Related to Front class data family.
-- Represents Message format of particular Frontend.
-- For example of instance see API.Telegram.Parse
data family FrontMessage :: Type

-- | Glue class to connect user representation, frontend methods calls,
-- Message data and bot logic.
-- All data you need to work with API should be stored inside frontHandle.
-- Consider this class as inner. So you don't need to provide logging functional
-- for methods of this class, unless you want to debug.
class ( BotUser user m idType
      , Monad m
      , Message FrontMessage)
   => Front frontHandle m user idType | user -> idType where
   getMessages     :: frontHandle -> m [(UserId user, ChatBot.Event FrontMessage)]
   -- ^ Gets list of new events from API.
   -- If you track offset, change it inside this funtion.
   sendResponse    :: frontHandle -> (UserId user, ChatBot.Response FrontMessage) -> m ()
   -- ^ Sends bot answer to the user
   createBotHandle :: frontHandle -> UserId user -> m (Maybe (ChatBot.Handle m))
   -- ^ Creates bot handle with provided @UserId@