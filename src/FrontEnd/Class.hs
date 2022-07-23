-- | Module contains class to work with different API by defined methods.
-- See FrontEnd.Telegram for instance example
-- General pattern to define instance of this class:
-- 1. Define @FrontMessage@ and parsers to construct @FrontMessage@ from raw data.
-- 2. With defined @FrontMessage@ make frontHandle data and it's instance of @Front@ class.
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
-- to-do
-- REWORK FRONT CLASS TO SUPPORT UPDATING OFFSET WITHOUT DEVIL TRICKS
-- Maybe on message function will do the job, or smthng like that
module FrontEnd.Class 
   (Front(..)) where

import Message (Message(..))
import qualified ChatBot (Response, Event)

-- | Class defines methods to get users input and send answers back.
-- All data you need to work with API should be stored inside frontHandle.
class ( Message frontMessage
      , Monad m)
   => Front frontHandle frontMessage userIdType m
      | frontHandle -> frontMessage, frontHandle -> userIdType where
   getMessages     :: frontHandle -> m [(userIdType, ChatBot.Event frontMessage)]
   -- ^ Gets list of new events.
   -- If you track offset, change it inside this funtion.
   sendResponse    :: frontHandle -> (userIdType, ChatBot.Response frontMessage) -> m ()
   -- ^ Sends bot answer to the user