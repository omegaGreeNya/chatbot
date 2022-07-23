-- | Telegram API methods constructors.
--    Provided functions do not make actual calls, they only construct Request type. 
{-# LANGUAGE DefaultSignatures #-}
-- to-do
-- Is Handle really should have hGetOffset function?
-- Test UNTESTED
module API.Telegram.Methods
   ( Handle(..)
   , Config(..)
   , getUpdates
   , sendMessage
   , sendSticker
   , fileTelegramRequest
   , getFile
   ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple ( Request
                           , setRequestMethod
                           , setRequestHost
                           , setRequestPath
                           , setRequestQueryString
                           , setRequestBodyJSON
                           , defaultRequest
                           )
import qualified Data.ByteString.Char8 as BS8

import qualified API.Telegram.Types as T

-- | Telegram API calls handle
data Handle m = Handle
   { hConfig    :: Config
   -- ^ Configurable data for making calls, url, token, etc.
   , hGetOffset :: m T.UpdateIdType  
   -- ^ This action provides message offset. 
   -- Offset used to inform telegram server from witch id updates we intrested in.
   -- Be careful thogth, telegram will never return update once it's id was less than offset.
   -- Offset should be changed only by caller logic.
   -- More info here: https://core.telegram.org/bots/api#getupdates
   }

data Config = Config
   { cfgHost    :: ByteString -- ^ Telegram api url (must be api.telegram.org)
   , cfgToken   :: ByteString -- ^ Bot token
   , cfgTimeout :: Int        -- ^ Time in seconds for long polling.
   } deriving Show

-- | Produce GET Request with empty body and query
defaultTelegramRequest :: Handle m       -- ^ Telegram methods constructor @Handle@
                       -> ByteString -- ^ Telegram method (e. "getUpdates")
                       -> Request        -- ^ 
defaultTelegramRequest h method = let
      token = cfgToken . hConfig $ h
      path = "/bot" <> token <> "/" <> method
   in setRequestHost (cfgHost . hConfig $ h)
   $  setRequestPath path
   $  defaultRequest



-- | Construct telegram @Request@ for geting updates. OK-Response contains list of up to 100 updates.
--    https://core.telegram.org/bots/api#getupdates
getUpdates :: (Monad m) => Handle m -> m Request
getUpdates h = do
   offset <- hGetOffset h
   let timeout = cfgTimeout . hConfig $ h
   return $ setRequestQueryString [("offset", packQVal offset), ("timeout", packQVal timeout)]
          $ defaultTelegramRequest h "getUpdates"

-- | Construct telegram @Request@ for sending message.
--    https://core.telegram.org/bots/api#sendmessage
sendMessage :: Handle m -> T.SendMessage -> Request
sendMessage h msg = 
   setRequestMethod "POST" -- actually not necessary for telegram
   $ setRequestBodyJSON msg -- header provided by this function
   $ defaultTelegramRequest h "sendMessage"

-- | Construct telegram @Request@ for sending Sticker from file.
--    https://core.telegram.org/bots/api#sendsticker   
-- UNTESTED
sendSticker :: Handle m -> T.SendSticker -> Request
sendSticker h sticker =
   setRequestMethod "POST" -- actually not necessary for telegram
   $ setRequestBodyJSON sticker -- !!!fix headers!!!
   $ defaultTelegramRequest h "sendMessage"

-- | Construct telegram @Request@.
-- Telegram returns object with file_id, file_unique_id, file_size, file_path.
--    https://core.telegram.org/bots/api#getfile
getFile :: Handle m  -- ^ Telegram methods constructor @Handle@
        -> Text      -- ^ file_id
        -> Request   -- ^ GET Request with empty body and query
getFile h fileId =
   setRequestQueryString [("file_id", packQVal fileId)]
   $ defaultTelegramRequest h "getFile"

-- | Produce GET Request. Telegram will answer with file data.
fileTelegramRequest :: Handle m  -- ^ Telegram methods constructor @Handle@
                    -> Text      -- ^ File path, provided by response (see https://core.telegram.org/bots/api#getfile)
                    -> Request   -- ^
fileTelegramRequest h filePath = let
      filePathBS = toBS filePath
      token = cfgToken . hConfig $ h
      path = "/file/bot" <> token <> "/" <> filePathBS
   in setRequestHost (cfgHost . hConfig $ h)
   $  setRequestPath path
   $  defaultRequest

-- | Type class for easy, dead-brain encoding ByteStrings
class ToByteString a where
   toBS :: a -> ByteString
   default toBS :: Show a => a -> ByteString
   toBS = BS8.pack . show

instance ToByteString Int
instance ToByteString Bool
instance ToByteString Text where
   toBS = encodeUtf8

-- | Helper function for packing query items.
packQVal :: ToByteString a => a -> Maybe ByteString
packQVal a = Just $ toBS a