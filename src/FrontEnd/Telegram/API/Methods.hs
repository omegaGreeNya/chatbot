-- | Telegram API methods constructors.
--    Provided functions do not make actual calls, they only construct Request type. 

-- to-do
-- Is Handle really should have hGetOffset function?
-- Im not setting any Headers, if sending json bodyes goes wrong, fixing headers might help.
-- !!!fix headers!!!
-- defaultTelegramRequest looks redundant
module FrontEnd.Telegram.API.Methods
   ( Handle
   , Config
   , getUpdates
   , sendMessage
   , sendFile
   )where

import Data.Aeson (Request)
import Network.HTTP.Simple (, defualtRequest)
import qualified Data.ByteString.Char8 as BS8

import qualified FrontEnd.Telegram.Types as T

-- | Telegram API calls handle
data Handle m = Handle
   { hConfig    :: Config -- ^ Configurable data for making calls, url, token, etc.
   , hGetOffset :: m Int  -- ^ This action provides message offset. 
                          --      Offset used to inform telegram server from witch id updates we intrested in.
                          --      Be careful thogth, telegram will never return update once it's id was less than offset.
                          --      offset should be changed by caller logic.
                          --      More info here: https://core.telegram.org/bots/api#getupdates
   }

data Config = Config
   { cfgHost    :: ByteString -- ^ Telegram api url (must be api.telegram.org)
   , cfgToken   :: ByteString -- ^ Bot token
   , cfgTimeout :: Int        -- ^ Time in seconds, to wait for long polls.
   } deriving Show

-- | Produce GET Request with empty body and query
defaultTelegramRequest :: Handle m       -- ^ Telegram methods constructor @Handle@
                       -> BS8.ByteString -- ^ Telegram method (e. "getUpdates")
                       -> Request        -- ^ 
defaultTelegramRequest h method = let
      token = cfgToken . hConfig $ h
      path = "/bot" <> token <> "/" <> method
   in setRequestHost (cfgHost . hConfig $ h)
   $  setRequestPath path
   $  defaultRequest


-- | Produce GET Request with empty body and query
fileTelegramRequest :: Handle m  -- ^ Telegram methods constructor @Handle@
                    -> Text      -- ^ File path, provided by response (see https://core.telegram.org/bots/api#getfile)
                    -> Request   -- ^ GET Request with empty body and query
fileTelegramRequest h filePath = let
      token = cfgToken . hConfig $ h
      path = "/file/bot" <> token <> "/" <> filePath
   in setRequestHost (cfgHost . hConfig $ h)
   $  setRequestPath path
   $  defaultRequest

-- | Construct telegram @Request@ for geting updates. OK-Response contains up to 100 updates.
--    https://core.telegram.org/bots/api#getupdates
getUpdates :: Handle m -> m Request
getUpdates h = do
   offset <- hGetOffset h
   let timeout = cfgTimeout . hConfig $ h
   return $ setRequestQueryString [("offset", Just offset), ("timeout", timeout)]
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
sendSticker :: Handle -> T.SendSticker -> Request
sendSticker h sticker =
   setRequestMethod "POST" -- actually not necessary for telegram
   $ setRequestBodyJSON sticker -- !!!fix headers!!!
   $ defaultTelegramRequest h "sendMessage"

-- | Construct telegram @Request@ for downloading file.
--    https://core.telegram.org/bots/api#getfile
getFile :: Handle m  -- ^ Telegram methods constructor @Handle@
        -> Text      -- ^ File path
        -> Request   -- ^ GET Request with empty body and query
getFile = fileTelegramRequest