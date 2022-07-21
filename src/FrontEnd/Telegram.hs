-- | Telegram frontend instance for Frint type class.
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
-- To-Do
-- Make standalone updateOffset function.
-- REWORK FRONT CLASS TO SUPPORT UPDATING OFFSET WITHOUT DEVIL TRICKS
module FrontEnd.Telegram where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Network.HTTP.Simple (httpLBS, getResponseBody)

import qualified Data.ByteString.Lazy.Char8 as B8 (unpack)
import qualified Data.Aeson as A (eitherDecode, encode)
import qualified Data.Text as T

import API.Telegram.ParseMessage (MessageTg(..), updateToMessage)
import FrontEnd.Class (Front(..))
import Lib ((.<), (.<~))
import Logger (logDebug, logError)
import User.Class (BotUser(..))
import User.Telegram (TelegramUser)

import qualified API.Telegram as API
import qualified ChatBot (Event(..))
import qualified Logger

type Offset = Int

data TelegramFrontHandle m = TelegramFrontHandle
   { hLogger       :: Logger.Handle m
   , hAPIHandle    :: API.Handle m
   , hUpdateOffset :: (Int -> Int) -> m ()
   }

instance (MonadIO m) => 
   Front (TelegramFrontHandle m) MessageTg (TelegramUser m) Int m where
      getMessages h = do
         -- construct request
         request <- API.getUpdates (hAPIHandle h)
         -- make request and get response
         responseRaw <- liftIO $ httpLBS request
         case A.eitherDecode $ getResponseBody responseRaw of
            -- sucessfuly parsed Json response
            Right response@(API.GetUpdates isOk results) -> do
               logDebug (hLogger h) $
                  "Successfully parsed " .< (length results) 
                  <> " Telegram updates"
               if isOk
               then
                  fmap catMaybes          -- We must parse all messages, but if parsers
                                          -- malformed, we log error and ignore that message
                  . mapM (produceEvent h) -- Construct events
                  . catMaybes             -- Cut off unparsed updates (bot ignores some of events)
                  . map updateToMessage   -- Construct messages
                  $ results
               else do -- i hope that would never happens
                  logError (hLogger h) $
                     "Telegram API method \"getUpdates\" failed. Response: " .<~ (A.encode response)
                     <> "\n      Raw responce was: " .<~ (T.pack . B8.unpack $ getResponseBody responseRaw)
                  return []
            -- error on parsing Json response
            Left msg -> do
               logError (hLogger h) $
                  "Parsing Telegram Update failed: " .<~ msg
               return []
      
      sendResponse = undefined
      
      createBotHandle = undefined

-- | Constructs Event. Should always return "Just _", beacouse
-- updateToMessage sieves updates that we ingnore, and we should be able
-- to construct event from every @MessageTg@,
-- ALSO this function updates offset if new update offset is bigger than current.
produceEvent :: (BotUser user idType m)
               => TelegramFrontHandle m 
               -> (idType, Offset, MessageTg)
               -> m (Maybe (UserId user, ChatBot.Event MessageTg))
produceEvent h (userId, newOffset, messageTg) = do
   userId' <- newUserId userId
   updateOffset h newOffset
   case messageToEvent messageTg of
      Left err -> do
         logError (hLogger h) err
         return Nothing
      Right event -> do
         logDebug (hLogger h) $
            "Succesfully parsed telegram message: " .<~ messageTg
         return $ Just (userId', event)

updateOffset :: Monad m
             => TelegramFrontHandle m
             -> Offset
             -> m ()
updateOffset h newOffset = do
   currentOffset <- (API.hGetOffset $ hAPIHandle h)
   when (currentOffset < newOffset) $ do
      logDebug (hLogger h) $
         "Setted new offset for Telegram API: " .< newOffset
      (hUpdateOffset h) (const newOffset)
   return ()

messageToEvent :: MessageTg -> Either Text (ChatBot.Event MessageTg)
messageToEvent (PressedButton buttonText) = 
   case readMaybe $ T.unpack buttonText of
      Just n -> Right $ ChatBot.SetRepetitionCountEvent n
      _      -> Left $ "Can't parse telegram pressed button event, callback text was: "
                     .<~ buttonText
messageToEvent msg = Right . ChatBot.MessageEvent $ msg