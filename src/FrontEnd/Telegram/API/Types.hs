{-# LANGUAGE DuplicateRecordFields #-}
-- Telegram API domain model
--    Model contains only necessary data, and provides "json -> ADT" constructors

-- To Do
-- Make To/FromJSON instances
-- !im actually not sure about this field!
module FrontEnd.Telegram.API.Types
   ( 
   ) where

! import Data.Aeson.Lens (Value(..), key, ..)
import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)

-- | Incoming update. AT MOST ONE of the optional parameters can be present in single update.
--    https://core.telegram.org/bots/api/#update
data Update = Update
   { update_id      :: Int                 -- ^ Unique update identifier
   , message        :: Maybe Message       -- ^ Text message update
   , callback_query :: Maybe CallbackQuery -- ^ Button press update 
   } deriving (Show) 

{-
instance FromJSON Update where
.. 

instance ToJSON Update where
.. 
-}

-- | Telegram message.
--    https://core.telegram.org/bots/api/#message
data Message = Message
   { message_id   :: Int                        -- ^ Unique message identifier
   , from         :: Maybe User                 -- ^ User who sent the message
   , chat         :: Chat                       -- ^ Chat message belongs to
   , text         :: Maybe Text                 -- ^ Message text
   , sticker      :: Maybe Sticker              -- ^ Message sticker
   , entities     :: [MessageEntity]            -- ^ Special entities for text messages, basically text markup.
   , reply_markup :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard
   } deriving (Show)

-- | Inline keyboard, optional field for Message, represents buttons with specified action on press.
--    https://core.telegram.org/bots/api/#inlinekeyboardmarkup
data InlineKeyboardMarkup = InlineKeyboardMarkup
   { inline_keyboard :: [[InlineKeyboardButton]]  -- ^ Array of arrays of key-buttons. Each inner array is a row in keyboard.
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

-- | One button of inline keyboard. We always intrested in callback query, but there may be present other actions on press.
--    https://core.telegram.org/bots/api/#inlinekeyboardbutton
data InlineKeyboardButton = InlineKeyboardButton
   { keyText       :: Text  -- ^ Key text
   , callback_data :: Text  -- ^ Data to be sent on key press
   } deriving (Show)

-- | Update data field, represents incoming query from callback button in an inline_keyboad.
--    https://core.telegram.org/bots/api/#callbackquery
data CallbackQuery = CallbackQuery
   { id   :: Text -- ^ Unique query id 
   , from :: User -- ^ User, that sended callback query
   } deriving (Show)

-- | Telegram user data
--    https://core.telegram.org/bots/api/#user
data User = User
   { id :: Int -- ^ Unique user id
   } deriving (Show)

-- | Telegram chat data
--    https://core.telegram.org/bots/api/#chat
data Chat = Chat
   { id :: Int -- ^ Unique chat id
   } deriving (Show)

-- | Entities for text message, usertags, markup, hashtags, etc
--    https://core.telegram.org/bots/api/#messageentity
data MessageEntity = MessageEntity
   { msgType  :: Text        -- ^ Entity type
   , offset   :: Int         -- ^ Start position of entity
   , length   :: Int         -- ^ Length of entity
   , url      :: Maybe Text  -- ^ For "text_link” entities only, URL that will be opened after user taps on the text
   , user     :: Maybe User  -- ^ For “text_mention” entities only, the mentioned user
   , language :: Maybe Text  -- ^ For “pre” entities only, the programming language of the entity text
   } deriving (Show)

-- | Telegram sticker data
--    https://core.telegram.org/bots/api/#sticker
data Sticker = Sticker
   { file_id           :: Text         -- ^ Sticker id for downloading/resending
   , file_unique_id    :: Text         -- ^ Absolutely unique id, not for downloading or resending
   , width             :: Int          -- ^ Sticker width
   , height            :: Int          -- ^ Sticker height    
   , is_animated       :: Bool         -- ^ True for animated sticker (https://telegram.org/blog/animated-stickers)
   , is_video          :: Bool         -- ^ True for video sticker (https://telegram.org/blog/video-stickers-better-reactions)
   , emoji             :: Maybe Text   -- ^ Emoji associated with the sticker
   , set_name          :: Maybe Text   -- ^ Name of the sticker set to which the sticker belongs
   , premium_animation :: Maybe File   -- ^ Premium animation for the sticker, if the sticker is premium
   , mask_position     :: MaskPosition -- ^ For mask stickers, the position where the mask should be placed
   , file_size         :: Int          -- ^ File size in bytes
   } deriving (Show)

-- | This object represents a file ready to be downloaded. 
--    The file can be downloaded via the link https://api.telegram.org/file/bot<token>/<file_path>.
--    It is guaranteed that the link will be valid for at least 1 hour. When the link expires, a new one can be requested by calling getFile.
--    https://core.telegram.org/bots/api/#file
data File = File
   { file_id        :: Text       -- ^ Identifier for this file, which can be used to download or reuse the file
   , file_unique_id :: Text       -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
   , file_size      :: Maybe Int  -- ^ File size in bytes. It can be bigger than 2^31 and some programming languages may have difficulty/silent defects in interpreting it. 
                                  --    But it has at most 52 significant bits, so a signed 64-bit integer or double-precision float type are safe for storing this value.
   , file_path      :: Maybe Text -- ^ File path. Use https://api.telegram.org/file/bot<token>/<file_path> to get the file.
   } deriving (Show)


-- im not ganna test this, just beacouse.
-- | This object describes the position on faces where a mask should be placed by default.
--    https://core.telegram.org/bots/api/#maskposition
data MaskPosition = MaskPosition
   { point   :: Text  -- ^ The part of the face relative to which the mask should be placed. One of “forehead”, “eyes”, “mouth”, or “chin”.
   , x_shift :: Float -- ^ Shift by X-axis measured in widths of the mask scaled to the face size, from left to right. For example, choosing -1.0 will place mask just to the left of the default mask position.
   , y_shift :: Float -- ^ Shift by Y-axis measured in heights of the mask scaled to the face size, from top to bottom. For example, 1.0 will place the mask just below the default mask position.
   , scale   :: Float -- ^ Mask scaling coefficient. For example, 2.0 means double size.
   } deriving (Show)

-- Helpful type to construct methods with json Request Bodies.
--   Same, they only cares about necessary data

-- | sendMessage method data
--    https://core.telegram.org/bots/api#sendmessage
data SendMessage = SendMessage
   { chat_id      :: Int
   , text         :: Text
   , reply_markup :: Maybe InlineKeyboardMarkup
   } deriving (Show)

data SendMessage = SendMessage
   { chat_id      :: Int
   , sticker      :: FilePath -- !im actually not sure about this field!
   } deriving (Show)
