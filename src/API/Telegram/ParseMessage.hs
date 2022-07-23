{-# LANGUAGE TypeFamilies #-}
-- to-do
-- Add stickers
module API.Telegram.ParseMessage 
   ( MessageTg (..)
   , updateToMessage
   ) where

import Control.Applicative ((<|>))
import Data.Text (Text)

import Lib ((.<~))
import Message (Message(..))
import qualified API.Telegram.Types as API

data MessageTg
   = TextMessageTg
      { messageText      :: Text
      , message_entities :: Maybe [API.MessageEntity]
      }
   | StickerTg
      { sticker          :: API.Sticker
      }
   | PressedButton
      { pressedButton    :: Text
      -- ^ This module do not parse Text to Int, since
      -- we want to log wrongly constructed callbacks
      }
   deriving (Show)

-- Get rid of pattern matching
instance Message MessageTg where
   messageToText msg@(TextMessageTg _ _) = Just $ messageText msg
   messageToText     (StickerTg     _  ) = Nothing
   messageToText msg@(PressedButton _  ) = Just $ "Pressed Button: " .<~ (pressedButton msg)
   textToMessage text = TextMessageTg text Nothing
   modifyMessage      = id

type UserId = Int
type UpdateId = Int

-- | Parses update to fully packed message.
-- Sender, offset, message itself.
updateToMessage :: API.Update -> Maybe (UpdateId, UserId, MessageTg)
updateToMessage upd = case parseResult of
   Just (userId, msg) -> Just (updateId, userId, msg)
   _                  -> Nothing 
   where
      updateId = API.upd_id upd
      parseResult
         =   asSendedMessage upd
         -- <|> asSendedSticker upd
         <|> asPressedButton upd

-- | Parse Update as if it was sended message
asSendedMessage :: API.Update -> Maybe (UserId, MessageTg)
asSendedMessage update = do
   message  <- API.upd_message update
   msg      <- API.msg_text message
   user     <- API.msg_from message
   let userId = API.user_id user
       entities = API.msg_entities message
   return (userId, TextMessageTg msg entities)

{-
-- | Parse Update as if it was sended sticker
asSendedSticker :: API.Update -> Maybe (UserId, MessageTg)
asSendedSticker = do 
   message  <- API.upd_message update
   msg      <- API.msg_sticker
   user     <- API.msg_from message
   let userId = API.user_id user
   return (userId, msg)
-}

-- | Parse Update as if it was pressed button event
asPressedButton :: API.Update -> Maybe (UserId, MessageTg)
asPressedButton update = do
   callbackQuery   <- API.upd_callback_query update
   callbackMessage <- API.callQ_message callbackQuery
   button          <- API.msg_text callbackMessage
   let userId = API.user_id $ API.callQ_from callbackQuery
   return (userId, PressedButton button)