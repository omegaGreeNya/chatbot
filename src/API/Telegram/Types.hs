-- | Telegram API domain model
--    Model contains only necessary data, and provides ToJSON/FromJSON instances
-- Record fields of data structures follows telegram API namings with underscore in front
-- Like so: _<API FIELD NAME>
{-# LANGUAGE TemplateHaskell       #-}

-- To Do
-- Test encoding decoding
module API.Telegram.Types
   where

import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)

import API.Telegram.DerivingExt (telegramDerivingDrop)

-- | Incoming update. AT MOST ONE of the optional parameters can be present in single update.
--    https://core.telegram.org/bots/api/#update
data Update = Update
   { upd_id             :: Int                 -- ^ Unique update identifier
   , upd_message        :: Maybe Message       -- ^ Text message update
   , upd_callback_query :: Maybe CallbackQuery -- ^ Button press update 
   } deriving (Show) 

-- | Telegram message.
--    https://core.telegram.org/bots/api/#message
data Message = Message
   { msg_id           :: Int                        -- ^ Unique message identifier
   , msg_from         :: Maybe User                 -- ^ User who sent the message
   , msg_chat         :: Chat                       -- ^ Chat message belongs to
   , msg_text         :: Maybe Text                 -- ^ Message text
   , msg_sticker      :: Maybe Sticker              -- ^ Message sticker
   , msg_entities     :: Maybe [MessageEntity]      -- ^ Special entities for text messages, basically text markup.
   , msg_reply_markup :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard
   } deriving (Show)

-- | Inline keyboard, optional field for Message, represents buttons with specified action on press.
--    https://core.telegram.org/bots/api/#inlinekeyboardmarkup
data InlineKeyboardMarkup = InlineKeyboardMarkup
   { inline_keyboard :: [[InlineKeyboardButton]]  -- ^ Array of arrays of key-buttons. Each inner array is a row in keyboard.
   } deriving (Show)

-- | One button of inline keyboard. We always intrested in callback query, but there may be present other actions on press.
--    https://core.telegram.org/bots/api/#inlinekeyboardbutton
data InlineKeyboardButton = InlineKeyboardButton
   { button_text          :: Text        -- ^ Key text
   , button_callback_data :: Maybe Text  -- ^ Data to be sent on key press.
                                         -- In our case it always Just Text, if not - it's a bug.
   } deriving (Show)

-- | Update data field, represents incoming query from callback button in an inline_keyboad.
--    https://core.telegram.org/bots/api/#callbackquery
data CallbackQuery = CallbackQuery
   { callQ_id      :: Text          -- ^ Unique query id 
   , callQ_from    :: User          -- ^ User, that sended callback query
   , callQ_message :: Maybe Message -- ^ Message from callback button.
   } deriving (Show)

-- | Telegram user data
--    https://core.telegram.org/bots/api/#user
data User = User
   { user_id :: Int -- ^ Unique user id
   } deriving (Show)

-- | Telegram chat data
--    https://core.telegram.org/bots/api/#chat
data Chat = Chat
   { chat_id :: Int -- ^ Unique chat id
   } deriving (Show)

-- | Entities for text message, usertags, markup, hashtags, etc
--    https://core.telegram.org/bots/api/#messageentity
data MessageEntity = MessageEntity
   { me_type     :: Text        -- ^ Entity type
   , me_offset   :: Int         -- ^ Start position of entity
   , me_length   :: Int         -- ^ Length of entity
   , me_url      :: Maybe Text  -- ^ For "text_link” entities only, URL that will be opened after user taps on the text
   , me_user     :: Maybe User  -- ^ For “text_mention” entities only, the mentioned user
   , me_language :: Maybe Text  -- ^ For “pre” entities only, the programming language of the entity text
   } deriving (Show)

-- | Telegram sticker data
--    https://core.telegram.org/bots/api/#sticker
data Sticker = Sticker
   { sticker_file_id           :: Text         -- ^ Sticker id for downloading/resending
   , sticker_file_unique_id    :: Text         -- ^ Absolutely unique id, not for downloading or resending
   , sticker_width             :: Int          -- ^ Sticker width
   , sticker_height            :: Int          -- ^ Sticker height    
   , sticker_is_animated       :: Bool         -- ^ True for animated sticker (https://telegram.org/blog/animated-stickers)
   , sticker_is_video          :: Bool         -- ^ True for video sticker (https://telegram.org/blog/video-stickers-better-reactions)
   , sticker_emoji             :: Maybe Text   -- ^ Emoji associated with the sticker
   , sticker_set_name          :: Maybe Text   -- ^ Name of the sticker set to which the sticker belongs
   , sticker_premium_animation :: Maybe File   -- ^ Premium animation for the sticker, if the sticker is premium
   , sticker_mask_position     :: MaskPosition -- ^ For mask stickers, the position where the mask should be placed
   , sticker_file_size         :: Int          -- ^ File size in bytes
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
   { maskPos_point   :: Text  -- ^ The part of the face relative to which the mask should be placed. One of “forehead”, “eyes”, “mouth”, or “chin”.
   , maskPos_x_shift :: Float -- ^ Shift by X-axis measured in widths of the mask scaled to the face size, from left to right. For example, choosing -1.0 will place mask just to the left of the default mask position.
   , maskPos_y_shift :: Float -- ^ Shift by Y-axis measured in heights of the mask scaled to the face size, from top to bottom. For example, 1.0 will place the mask just below the default mask position.
   , maskPos_scale   :: Float -- ^ Mask scaling coefficient. For example, 2.0 means double size.
   } deriving (Show)

-- WARN: IF YOU ADD NEW DATA, ADD IT'S INSTANCE HERE
-- I might rework this later, but for now it's ok..
--
-- Due template compication specific, all inner types
-- of templating structure should be defined in previous lines.
-- So i just write all the bunch here.
-- Ofcourse it's possible to re-arrenge them to solve conflicts,
-- But there may be presented some king of a loop (not checked).
deriveJSON (telegramDerivingDrop 4) ''Update
deriveJSON (telegramDerivingDrop 4) ''Message
deriveJSON (telegramDerivingDrop 0) ''InlineKeyboardMarkup
deriveJSON (telegramDerivingDrop 7) ''InlineKeyboardButton
deriveJSON (telegramDerivingDrop 6) ''CallbackQuery
deriveJSON (telegramDerivingDrop 5) ''User
deriveJSON (telegramDerivingDrop 5) ''Chat
deriveJSON (telegramDerivingDrop 3) ''MessageEntity
deriveJSON (telegramDerivingDrop 8) ''Sticker
deriveJSON (telegramDerivingDrop 0) ''File
deriveJSON (telegramDerivingDrop 8) ''MaskPosition
-- And sicnce methods use only data types above, it is save
-- to deriveJSON in more pleasant form, right after data definition

-- Helpful type to construct methods with json Request Bodies.
-- Same, they only cares about necessary data

data GetUpdates = GetUpdates
   { get_ok     :: Bool
   , get_result :: [Update]
   } deriving (Show)

deriveJSON (telegramDerivingDrop 4) ''GetUpdates

-- | sendMessage method data
-- https://core.telegram.org/bots/api#sendmessage
data SendMessage = SendMessage
   { sendMsg_chat_id      :: Int
   , sendMsg_text         :: Text
   , sendMsg_reply_markup :: Maybe InlineKeyboardMarkup
   } deriving (Show)

deriveJSON (telegramDerivingDrop 8) ''SendMessage

-- | sendSticker method data
-- https://core.telegram.org/bots/api#sendsticker
data SendSticker = SendSticker
   { sendStiker_chat_id      :: Int
   , sendStiker_sticker      :: Text -- !im actually not sure about this field!
   } deriving (Show)

deriveJSON (telegramDerivingDrop 11) ''SendSticker