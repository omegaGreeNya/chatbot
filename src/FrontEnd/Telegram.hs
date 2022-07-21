-- | Telegram frontend instance for Frint type class.
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
-- To-Do
-- Move some logic away, parsing particullary.
-- Make function parseUser based on hasField class.
-- Remove duplicated fields.
module FrontEnd.Telegram where
{-
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Lens (key)
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.IORef (IORef)
import Network.HTTP.Simple (httpLBS, getResponseBody)

import qualified Data.Aeson as A (eitherDecode, encode)
import qualified Data.Text as T

import FrontEnd.Class (Front(..))
import Lib ((.<~))
import Logger (logDebug, logError)
import Message (Message(..))
import User.Class (BotUser(..))
import User.Telegram (TelegramUser)

import qualified API.Telegram as API
import qualified ChatBot (Handle, Response(..), Event(..))
import qualified Logger

data TelegramFrontHandle m = TelegramFrontHandle
   { hLogger       :: Logger.Handle m
   , hAPIHandle    :: API.Handle m
   , hUpdateOffset :: (Int -> Int) -> m ()
   }

data State = State
   { currentOffset :: Int
   } deriving (Show)

instance (MonadIO m) => 
   Front (TelegramFrontHandle m) m (TelegramUser m) Int where
      data FrontMessage (TelegramFrontHandle m) 
         = MessageTg
            { updateId         :: Int
            , messageText      :: Text
            , message_entities :: Maybe [API.MessageEntity]
            }
         | StickerTg
            { updateId         :: Int
            , sticker          :: API.Sticker
            }
         deriving (Show)
      
      getMessages h = do
         request <- API.getUpdates (hAPIHandle h)
         responseRaw <- liftIO $ httpLBS request
         case A.eitherDecode $ getResponseBody responseRaw of
            Right response@(API.GetUpdates isOk results) -> do
               logDebug (hLogger h) $
                  "Successfully parsed Telegram Update"
               if isOk
               then
                  return . catMaybes $ map updateToMessage results
               else do
                  logError (hLogger h) $
                     "Telegram API method \"getUpdates\" failed. Response: " .<~ (A.encode response)
                  return []
            Left msg -> do
               logError (hLogger h) $
                  "Parsing Telegram Update failed: " .<~ msg
               return []
      
      sendResponse h (userId, (ChatBot.MenuResponse title buttons)) = 
         undefined
      
      sendResponse h (userId, (ChatBot.MessageResponse messageTg)) =
         undefined
      
      createBotHandle = undefined

instance MonadIO m => Message (FrontMessage (TelegramFrontHandle m)) where
   messageToText msg@(MessageTg _ _ _) = Just $ messageText msg
   messageToText msg@(StickerTg _ _  ) = Nothing
   textToMessage text = MessageTg 1 text Nothing
   modifyMessage      = id


-- | Parses update to @ChatBot.Message@
updateToMessage :: API.Update -> Maybe (UserId (TelegramUser m), ChatBot.Event (FrontMessage (TelegramFrontHandle m)))
updateToMessage update = asSendedMessage <|> asSendedSticker <|> asPressedButton
   where
      updateId = API._update_id update
      
      asSendedMessage = do -- Parse Update as if it was sended message
         message  <- (API._message :: API.Update -> Int) update
         text     <- API._text message
         user     <- API._from message
         let userId   = newUserId $ API._id user
             entities = API._entities message -- entities is optional field
             event    = ChatBot.MessageEvent $ MessageTg updateId text entities
         return (userId, event)
      
      asSendedSticker = do -- Parse Update as if it was sended sticker
         message  <- API._message update
         sticker  <- API._sticker message
         user     <- API._from message
         let userId = newUserId $ API._id user
             event =  ChatBot.MessageEvent $ StickerTg updateId sticker
         return (userId, event)
      
      asPressedButton = do -- You got the idea
         callbackQuery   <- API._callback_query update
         callbackMessage <- API._message callbackQuery
         nText           <- API._text callbackMessage
         n               <- readMaybe . T.unpack $ nText
         let userId = newUserId . API._id . API._from $ callbackQuery
         -- IF YOU MADE PARSEUSER BASED ON HASFIELD, TEST IT WITH CALLBACK QUERY
             event = SetRepetitionCountEvent n
-- Make a decision between httpLBS and httpBS.
         return (userId, event)
-}