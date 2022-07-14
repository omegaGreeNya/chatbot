-- | Telegram API methods constructors.
--    Provided functions do not make actual calls. Use !THIS! f

-- to-do
-- !THIS!
-- data Method types
-- !LINK!
-- !FIX ORDER!
-- !Make application/json for Method data!
module FrontEnd.Telegram.API.Methods
   ( Handle
   , Config
   , getUpdates
   , sendMessage
   )where

import FrontEnd.Telegram.
! import Data.ByteString (ByteString)
! import qualified Data.ByteString as B

-- | Telegram API calls handle
data Handle m = Handle
   { hConfig    :: Config -- ^ Configurable data for making calls, url, token, etc.
   , hGetOffset :: m Int  -- ^ This action provides message offset. 
                          --      Offset used to inform telegram server from witch id updates we intrested in.
                          --      Be careful thogth, telegram will never return update once it's id was less than offset.
                          --      offset should be changed by caller logic.
                          --      More info here: !LINK!
   }

data Config = Config
   { cfgUrl     :: ByteString -- ^ Telegram api url
   , cfgToken   :: ByteString -- ^ Bot token
   , cfgTimeout :: Int        -- ^ Time in seconds, to wait for long polls.
   } deriving Show

-- MOVE AWAY create module for making http-calls
-- | This data presents ready to send http-call
data Method = GET ByteString
            | POST ByteString

-- | Asks telegram server for updates
--    !LINK!
getUpdates :: Handle -> m Method
getUpdates h = do
   offset <- hGetOffset h
   let url = h & hConfig . cfgUrl
       token = h & hConfig . cfgToken
       timeout = h & hConfig . cfgTimeout
       -- !FIX function ORDER! --
   return . GET 
      $ url 
      <> token 
      <> "/getUpdates?" 
      <> "offset" =. offset
      <> "timeout" =. timeout

-- !Make application/json for Method data!
sendMessage :: Handle -> Text -> Maybe T.InlineKeyboardMarkup -> Method
sendMessage h text =
   let 
   in POST
   $ url
   <> token
   <> "/sendMessage?"
   <> "text" =. text
   <> undefined

-- MOVE AWAY
-- | This operator helps construct uri parametrs for http-calls
(=.) :: Show a -> ByteString -> a -> ByteString
(=.) parametr val = parametr <> "=" <> (B.encode . show $ val) <> "&"
infixr 7 (=.)