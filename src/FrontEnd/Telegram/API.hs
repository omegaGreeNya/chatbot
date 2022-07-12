{-# LANGUAGE DuplicateRecordFields #-}
-- Telegram API domain model
--    Model contains only necessary data, and provides "json -> ADT" constructors


module FrontEnd.Telegram.API
   ( 
   ) where

import Data.Text (Text)

-- Incoming update. AT MOST ONE of the optional parameters can be present in single update.
-- https://core.telegram.org/bots/api/#update
data Update = Update
   { update_id :: Int
   -- text message update
   , message :: Maybe Message
   -- button press update 
   , callback_query :: CallbackQuery
   } deriving (Show) 

-- Telegram message.
-- https://core.telegram.org/bots/api/#message
data Message = Message
   { message_id :: Int
   -- user that sended message
   , from :: Maybe User
   -- chat message belongs to
   , chat :: Chat
   -- message text
   , text :: Maybe Text
   , sticker :: Maybe Sticker
   -- special entities for text messages, basically text markup.
   , entities :: [MessageEntity]
   , reply_markup :: Maybe InlineKeyboardMarkup
   } deriving (Show)

-- Inline keyboard, optional field for Message, represents buttons with specified action on press.
-- https://core.telegram.org/bots/api/#inlinekeyboardmarkup
data InlineKeyboardMarkup = InlineKeyboardMarkup
   { inline_keyboard :: [[InlineKeyboardButton]]
   } deriving (Show)
{-
EXAMPLE
POST /bot<TOKEN>/sendMessage HTTP/1.1
Host: api.telegram.org
Accept: application/json
Content-Type: application/json
Content-Length: 142

{ 
  "chat_id":628639571
, "text":"ChooseButton"
, "reply_markup":
 { 
   "inline_keyboard":[[{"text":"1","callback_data":"data"}]]
 }
}
-}
-- One button of inline keyboard. We always use callback query.
-- https://core.telegram.org/bots/api/#inlinekeyboardbutton
data InlineKeyboardButton = InlineKeyboardButton
   { keyText :: Text
   , callback_data :: Text
   } deriving (Show)

-- Optional update, represents incoming query from callback button in an inline_keyboad.
-- https://core.telegram.org/bots/api/#callbackquery
data CallbackQuery = CallbackQuery
   { id :: Text
   , from :: User
   } deriving (Show)

-- Telegram user data
-- https://core.telegram.org/bots/api/#user
data User = User
   { id :: Int
   } deriving (Show)

-- Telegram chat data
-- https://core.telegram.org/bots/api/#chat
data Chat = Chat
   { id :: Int
   } deriving (Show)

-- Entities for text message, usertags, markup, hashtags, etc
-- https://core.telegram.org/bots/api/#messageentity
data MessageEntity = MessageEntity
   { messageEntityType :: Text
   , offset :: Int
   , length :: Int
   , url :: Maybe Text
   , user :: Maybe User
   , language :: Maybe Text
   } deriving (Show)

data Sticker = Sticker
   { file_id :: Text
   , file_unique_id :: Text
   , width :: Int
   , height :: Int
   , is_animated :: Bool
   , is_video :: Bool
   --, thumb :: Bool
   , emoji :: Maybe Text
   , set_name :: Maybe Text
   --, premium_animation :: Maybe File
   --, mask_position :: MaskPosition
   , file_size :: Int
   } deriving (Show)