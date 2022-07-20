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

-- | Glue class to connect user representation, frontend methods and bot logic.
-- This is inner class, you don't need to provide logging functional for methods of this class.
-- frontHandle should contain all info to construct BotHandle, with provided UserId.
-- Also, you can use frontHandle to store host adress, botkey, etc.
-- All data you need to work with API may be stored inside this handle.
class ( BotUser user m idType
      , Monad m
      , Message (FrontMessage frontHandle))
   => Front frontHandle m user idType | user -> idType where
   data FrontMessage frontHandle :: Type
   -- ^ Associated Message type, to work not only with message text, 
   -- but also with metadata about message (markup for example).
   getMessages     :: frontHandle -> m [(UserId user, ChatBot.Event (FrontMessage frontHandle))]
   -- ^ Gets list of new events from API.
   -- If you track offset, change it inside this funtion.
   sendResponse    :: frontHandle -> (UserId user, ChatBot.Response (FrontMessage frontHandle)) -> m ()
   -- ^ Sends bot answer to the user
   createBotHandle :: frontHandle -> UserId user -> m (Maybe (ChatBot.Handle m))
   -- ^ Creates bot handle with provided @UserId@