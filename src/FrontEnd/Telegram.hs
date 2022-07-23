-- | Telegram frontend instance for Frint type class.
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
-- To-Do
-- add logging into sendMessage
module FrontEnd.Telegram where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Network.HTTP.Simple (Request, httpBS, httpLBS, getResponseBody)

import qualified Data.ByteString.Lazy.Char8 as B8 (unpack)
import qualified Data.Aeson as A (eitherDecode, encode)
import qualified Data.Text as T

import API.Telegram (UpdateIdType, UserIdType)
import API.Telegram.ParseMessage (MessageTg(..), updateToMessage)
import FrontEnd.Class (Front(..))
import Lib ((.<), (.<~))
import Logger (logDebug, logError)

import qualified API.Telegram as API
import qualified ChatBot (Event(..), Response(..))
import qualified Logger


--type Offset = _ <- from module API.Telegram.Methods
-- type UserIdType = _ <- from module API.Telegram.ParseMessage 

data TelegramFrontHandle m = TelegramFrontHandle
   { hLogger       :: Logger.Handle m
   , hAPIConfig    :: API.Config
   , hGetOffset    :: m UpdateIdType
   , hUpdateOffset :: (UpdateIdType -> UpdateIdType) -> m ()
   }

hAPIHandle :: TelegramFrontHandle m -> API.Handle m
hAPIHandle h = API.Handle (hAPIConfig h) (hGetOffset h)

instance (MonadIO m) => 
   Front (TelegramFrontHandle m) MessageTg UserIdType m where
      -- | Gets data from telegram server, parses it, 
      -- packs output pairs and updates offset state if needed.
      getMessages h = do
         -- << Getting raw data from http request
         request <- API.getUpdates (hAPIHandle h)
         responseRaw <- liftIO $ httpLBS request
         -- >>
         -- << Parsing Results
         case A.eitherDecode $ getResponseBody responseRaw of
            -- Sucessfuly parsed Json response
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
            -- Error on parsing Json response
            Left msg -> do
               logError (hLogger h) $
                  "Parsing Telegram Update failed: " .<~ msg
               return []
         -- >>
      
      sendResponse h userId (ChatBot.MessageResponse msg) = do
         request <- messageToRequestTg h userId msg
         _ <- liftIO $ httpBS request
         return ()
      sendResponse h userId (ChatBot.MenuResponse title buttons) = do
         let keyboard = rawButtonsToKeyboard buttons
             msg = (API.defaultSendMessage userId title)
                     {API.sendMsg_reply_markup = Just keyboard}
         request <- API.sendMessage (hAPIHandle h) msg
         _ <- liftIO $ httpBS request
         return ()

-- | Constructs Event and updates Offset. 
-- Should always return "Just _", 
-- because updateToMessage sieves updates that we ingnore,
-- so we should be able to construct event from every @MessageTg@.
produceEvent :: Monad m
             => TelegramFrontHandle m 
             -> (UpdateIdType, UserIdType, MessageTg)
             -> m (Maybe (UserIdType, ChatBot.Event MessageTg))
produceEvent h (newOffset, userId, messageTg) = do
   updateOffset h newOffset
   case messageToEvent messageTg of
      Left err -> do
         logError (hLogger h) err
         return Nothing
      Right event -> do
         logDebug (hLogger h) $
            "Succesfully parsed telegram message: " .<~ messageTg
         return $ Just (userId, event)

-- | Updates offset if new update is bigger than current offset
updateOffset :: Monad m
             => TelegramFrontHandle m
             -> UpdateIdType
             -> m ()
updateOffset h newOffset = do
   currentOffset <- (hGetOffset h)
   when (currentOffset < newOffset) $ do
      logDebug (hLogger h) $
         "Setted new offset for Telegram API: " .< newOffset
      (hUpdateOffset h) (const newOffset)
   return ()

-- | Constructs Event.
-- Error may happen on parsing pressed button event.
-- If so, returns Left <error text>, otherwise Right event.
messageToEvent :: MessageTg -> Either Text (ChatBot.Event MessageTg)
messageToEvent (PressedButton buttonText) = 
   case readMaybe $ T.unpack buttonText of
      Just n -> Right $ ChatBot.SetRepetitionCountEvent n
      _      -> Left $ "Can't parse telegram pressed button event, callback text was: "
                     .<~ buttonText
messageToEvent msg = Right . ChatBot.MessageEvent $ msg


-- | Forms list of Ints into InlineKeyboard
-- with buttons grouped by 3 in a row.
rawButtonsToKeyboard :: Show a => [a] -> API.InlineKeyboardMarkup
rawButtonsToKeyboard = API.InlineKeyboardMarkup . rowsByN 3 . map formButton

rowsByN :: Int -> [a] -> [[a]]
rowsByN n buttons =
   case splitAt n buttons of
      (x , []) -> [x]
      (x , xs) -> x : (rowsByN n xs)

formButton :: Show a => a -> API.InlineKeyboardButton
formButton x = let button = T.pack . show $ x
   in API.InlineKeyboardButton button (Just button)


messageToRequestTg :: MonadIO m
                   => TelegramFrontHandle m
                   -> UserIdType
                   -> MessageTg
                   -> m Request
messageToRequestTg h userId (TextMessageTg text entities) = do
   let msg = (API.defaultSendMessage userId text)
               {API.sendMsg_entities = entities}
   API.sendMessage (hAPIHandle h) msg
messageToRequestTg h userId (StickerTg sticker') = do
   let file = API.sticker_file_id sticker'
       stickerToSend = API.SendSticker userId file
   API.sendSticker (hAPIHandle h) stickerToSend
messageToRequestTg h userId (PressedButton _) = 
   -- logging here
   undefined