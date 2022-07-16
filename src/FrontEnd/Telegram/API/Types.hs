{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
-- Telegram API domain model
--    Model contains only necessary data, and provides "json -> ADT" constructors
-- Record fields of data structures follows telegram API namings with underscore in front


-- To Do
-- Test encoding decoding
-- !im actually not sure about this field!
module FrontEnd.Telegram.API.Types
   ( 
   ) where

import Data.Aeson.TH (Options (..), defaultOptions, deriveJSON)
import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)

telegramDerivingOptions :: Options
telegramDerivingOptions = defaultOptions
   { fieldLabelModifier = drop 1
   , omitNothingFields = True
   }

-- | Incoming update. AT MOST ONE of the optional parameters can be present in single update.
--    https://core.telegram.org/bots/api/#update
data Update = Update
   { _update_id      :: Int                 -- ^ Unique update identifier
   , _message        :: Maybe Message       -- ^ Text message update
   , _callback_query :: Maybe CallbackQuery -- ^ Button press update 
   } deriving (Show) 

deriveJSON telegramDerivingOptions ''Update

-- | Telegram message.
--    https://core.telegram.org/bots/api/#message
data Message = Message
   { _message_id   :: Int                        -- ^ Unique message identifier
   , _from         :: Maybe User                 -- ^ User who sent the message
   , _chat         :: Chat                       -- ^ Chat message belongs to
   , _text         :: Maybe Text                 -- ^ Message text
   , _sticker      :: Maybe Sticker              -- ^ Message sticker
   , _entities     :: [MessageEntity]            -- ^ Special entities for text messages, basically text markup.
   , _reply_markup :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard
   } deriving (Show)

deriveJSON telegramDerivingOptions ''Message

-- | Inline keyboard, optional field for Message, represents buttons with specified action on press.
--    https://core.telegram.org/bots/api/#inlinekeyboardmarkup
data InlineKeyboardMarkup = InlineKeyboardMarkup
   { _inline_keyboard :: [[InlineKeyboardButton]]  -- ^ Array of arrays of key-buttons. Each inner array is a row in keyboard.
   } deriving (Show)

deriveJSON telegramDerivingOptions ''Message


-- | One button of inline keyboard. We always intrested in callback query, but there may be present other actions on press.
--    https://core.telegram.org/bots/api/#inlinekeyboardbutton
data InlineKeyboardButton = InlineKeyboardButton
   { _text          :: Text        -- ^ Key text
   , _callback_data :: Maybe Text  -- ^ Data to be sent on key press.
                                  -- In our case it always Just Text, if not - it's a bug.
   } deriving (Show)

deriveJSON telegramDerivingOptions ''InlineKeyboardButton

-- | Update data field, represents incoming query from callback button in an inline_keyboad.
--    https://core.telegram.org/bots/api/#callbackquery
data CallbackQuery = CallbackQuery
   { _id   :: CallbackQuery_id -- ^ Unique query id 
   , _from               :: User             -- ^ User, that sended callback query
   } deriving (Show)

deriveJSON telegramDerivingOptions ''CallbackQuery

-- | Telegram user data
--    https://core.telegram.org/bots/api/#user
data User = User
   { _id :: Int -- ^ Unique user id
   } deriving (Show)

deriveJSON telegramDerivingOptions ''User

-- | Telegram chat data
--    https://core.telegram.org/bots/api/#chat
data Chat = Chat
   { _id :: Int -- ^ Unique chat id
   } deriving (Show)

deriveJSON telegramDerivingOptions ''Chat

-- | Entities for text message, usertags, markup, hashtags, etc
--    https://core.telegram.org/bots/api/#messageentity
data MessageEntity = MessageEntity
   { _type     :: Text        -- ^ Entity type
   , _offset   :: Int         -- ^ Start position of entity
   , _length   :: Int         -- ^ Length of entity
   , _url      :: Maybe Text  -- ^ For "text_link” entities only, URL that will be opened after user taps on the text
   , _user     :: Maybe User  -- ^ For “text_mention” entities only, the mentioned user
   , _language :: Maybe Text  -- ^ For “pre” entities only, the programming language of the entity text
   } deriving (Show)
   
deriveJSON telegramDerivingOptions ''MessageEntity

-- | Telegram sticker data
--    https://core.telegram.org/bots/api/#sticker
data Sticker = Sticker
   { _file_id           :: Text         -- ^ Sticker id for downloading/resending
   , _file_unique_id    :: Text         -- ^ Absolutely unique id, not for downloading or resending
   , _width             :: Int          -- ^ Sticker width
   , _height            :: Int          -- ^ Sticker height    
   , _is_animated       :: Bool         -- ^ True for animated sticker (https://telegram.org/blog/animated-stickers)
   , _is_video          :: Bool         -- ^ True for video sticker (https://telegram.org/blog/video-stickers-better-reactions)
   , _emoji             :: Maybe Text   -- ^ Emoji associated with the sticker
   , _set_name          :: Maybe Text   -- ^ Name of the sticker set to which the sticker belongs
   , _premium_animation :: Maybe File   -- ^ Premium animation for the sticker, if the sticker is premium
   , _mask_position     :: MaskPosition -- ^ For mask stickers, the position where the mask should be placed
   , _file_size         :: Int          -- ^ File size in bytes
   } deriving (Show)

deriveJSON telegramDerivingOptions ''Sticker

-- | This object represents a file ready to be downloaded. 
--    The file can be downloaded via the link https://api.telegram.org/file/bot<token>/<file_path>.
--    It is guaranteed that the link will be valid for at least 1 hour. When the link expires, a new one can be requested by calling getFile.
--    https://core.telegram.org/bots/api/#file
data File = File
   { _file_id        :: Text       -- ^ Identifier for this file, which can be used to download or reuse the file
   , _file_unique_id :: Text       -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
   , _file_size      :: Maybe Int  -- ^ File size in bytes. It can be bigger than 2^31 and some programming languages may have difficulty/silent defects in interpreting it. 
                                  --    But it has at most 52 significant bits, so a signed 64-bit integer or double-precision float type are safe for storing this value.
   , _file_path      :: Maybe Text -- ^ File path. Use https://api.telegram.org/file/bot<token>/<file_path> to get the file.
   } deriving (Show)

deriveJSON telegramDerivingOptions ''File

-- im not ganna test this, just beacouse.
-- | This object describes the position on faces where a mask should be placed by default.
--    https://core.telegram.org/bots/api/#maskposition
data MaskPosition = MaskPosition
   { _point   :: Text  -- ^ The part of the face relative to which the mask should be placed. One of “forehead”, “eyes”, “mouth”, or “chin”.
   , _x_shift :: Float -- ^ Shift by X-axis measured in widths of the mask scaled to the face size, from left to right. For example, choosing -1.0 will place mask just to the left of the default mask position.
   , _y_shift :: Float -- ^ Shift by Y-axis measured in heights of the mask scaled to the face size, from top to bottom. For example, 1.0 will place the mask just below the default mask position.
   , _scale   :: Float -- ^ Mask scaling coefficient. For example, 2.0 means double size.
   } deriving (Show)

deriveJSON telegramDerivingOptions ''MaskPosition

-- Helpful type to construct methods with json Request Bodies.
--   Same, they only cares about necessary data

-- | sendMessage method data
--    https://core.telegram.org/bots/api#sendmessage
data SendMessage = SendMessage
   { _chat_id      :: Int
   , _text         :: Text
   , _reply_markup :: Maybe InlineKeyboardMarkup
   } deriving (Show)

deriveJSON telegramDerivingOptions ''SendMessage

data SendSticker = SendSticker
   { _chat_id      :: Int
   , _sticker      :: FilePath -- !im actually not sure about this field!
   } deriving (Show)

deriveJSON telegramDerivingOptions ''SendSticker