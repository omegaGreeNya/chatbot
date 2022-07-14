{-# LANGUAGE DuplicateRecordFields #-}
-- Telegram API domain model
--    Model contains only necessary data, and provides "json -> ADT" constructors

-- To Do
-- <!!!INSERT TYPE NAME HERE>
-- ?? complite missing description
-- ?Object?
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

-- <<

instance FromJson Update where
.. 

instance ToJson Update where
.. 

-- choose realization /\ OR \/

update :: ByteString -> Maybe Update
update = _Update $ ?Object?

_Update :: Value -> Maybe Update
_Update val = val & key "update_id" . 

-- >>

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
   { id   :: Text -- ^ Query id (?? unique ??)
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
   , url      :: Maybe Text  -- ^ Only for <!!!INSERT TYPE NAME HERE> entities
   , user     :: Maybe User  -- ^ Only for <!!!INSERT TYPE NAME HERE> entities
   , language :: Maybe Text  -- ^ Only for <!!!INSERT TYPE NAME HERE> entities
   } deriving (Show)

-- | Telegram sticker data
--    ?? link ??
data Sticker = Sticker
   { file_id        :: Text  -- ^ Sticker id for downloading
   , file_unique_id :: Text  -- ^ Sticker id for sending without upload
   , width          :: Int   -- ^ Sticker image width
   , height         :: Int   -- ^ Sticker image height    
   , is_animated    :: Bool  -- ^ True for animated sticker
   , is_video       :: Bool  -- ^ ?? True for video sticker ??
   -- ??, thumb :: Bool
   , emoji          :: Maybe Text -- ^ ?? ??
   , set_name       :: Maybe Text -- ^ ?? ??
   -- ??, premium_animation :: Maybe File
   -- ??, mask_position :: MaskPosition
   , file_size :: Int -- ^ Sticker file size (??)
   } deriving (Show)