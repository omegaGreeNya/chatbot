-- | Telegram bot application runner

module FrontEnd.Telegram
   ( 
   ) where
{-
import Control.Lens (preview)
import Control.Monad (mapM, _mapM)
--import Data.List (filter)
import Data.Aeson.Lens ( key, _String )
import Data.ByteString.Char8 (ByteString)
import Data.Either (isRight)
import Network.HTTP.Simple (getResponseBody, httpBS)

import Lib
import qualified ChatBot

data Handle = Handle
   { hConfig :: Config
   , hToken :: String
   , hLogger :: Logger.Handle
   , hGetState  :: IO State
   }

data Config = Config
   { cfgOffset :: Int
   , cfgTimeout :: Int
   } deriving (Show)

data State = State
   { usersConfigs :: Map User.Id User.Config

   -- get update  (getUpdates?offset)
   -- generate response
   -- post response (sendMessage&chat_id=,)
run :: Handle -> IO ()
run h = do
   let loop = do
         updates <- getUpdates h
         let events = filter isRight . map parseUpdate updates
         responses <- mapM . generateResponse h $ events
         _mapM . postResponse h $ responses
   _ <- loop

getUpdates :: Handle -> IO ByteString
getUpdates h = do
   result <- httpBS $ parseRequest "GET https://api.telegram.org/bot" <> (hToken h) <> "/getUpdates"
   return $ getResponseBody result

getMessage :: ?????????? -> Maybe Text
getMessage = undefined

toListOf (key "result" . _Array . each . key "message" . key "text") json

parseUpdate :: Handle -> ByteString -> IO ParsedUpdate
parseUpdate h json = do
   let messages_texts = json ^.. key "result" . _Array . each . key "message" . key "text"
   let chat_ids = json ^.. key "result" . _Array . each . key "message" . key "chat" . key "id"
   

generateResponse :: Handle -> 
generateResponse

postResponse :: Handle -> ChatBot.Response TelegramMessage
postResponse = undefined
-}