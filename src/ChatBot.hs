-- | Bot interface and logic

-- To-Do
-- add Multimedia?
-- ?Move interface away, this is internal module
module ChatBot 
   ( Handle (..)
   , Config (..)
   , State (..)
   , Event (..)
   , Response (..)
   , RepetitionCount
   , respond
   ) where

import Control.Monad (replicateM)
import Data.Aeson
--import Data.List (singleton)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Text as T 

import Lib ((.<), singleton)
import Message
import qualified Logger

data Handle m = Handle
   { hConfig :: Config
   , hLoggerHandle :: Logger.Handle m
   , hGetState :: m State
   , hModifyState :: (State -> State) -> m ()
   }

data Config = Config
   { cfgHelpText              :: Text
   , cfgRepeatText            :: Text
   , cfgMaxRepetitionsCount   :: RepetitionCount
   , cfgDefaultRepeatCount    :: RepetitionCount
   } deriving (Generic, Show)

instance ToJSON Config where
   toEncoding = genericToEncoding defaultOptions

instance FromJSON Config
   
data State = State
   { stateRepetitionCount :: RepetitionCount
   } deriving Show

-- /\ smart constructors? /\

data Event a = SetRepetitionCountEvent RepetitionCount
             | MessageEvent a
             deriving Show

data Response a = MessageResponse a
                | MenuResponse Title [(RepetitionCount, Event a)]
                deriving Show

type Title = Text

type RepetitionCount = Int

respond :: (Monad m, Message a) => Handle m -> Event a -> m [Response a]
respond h (SetRepetitionCountEvent n) = 
   handleChangeRepetitionCount h n
respond h (MessageEvent msg) 
   | isCommand msg "/help"   = respondHelpCommand h
   | isCommand msg "/repeat" = respondRepeatCommnad h
   | otherwise               = respondOnMessage h msg

isCommand :: Message a => a -> Text -> Bool
isCommand msg cmd = case messageToText msg of
   Nothing      -> False
   Just msgText -> msgText == cmd

handleChangeRepetitionCount :: Monad m => Handle m -> Int -> m [Response a]
handleChangeRepetitionCount h n = do
   Logger.logInfo (hLoggerHandle h) $ "User setted repetition count to: " <> (T.pack . show $ n)
   hModifyState h (\s -> s{stateRepetitionCount = n})
   return []

-- singleton or []?
respondHelpCommand :: (Monad m, Message a) => Handle m -> m [Response a]
respondHelpCommand h = do
   Logger.logInfo (hLoggerHandle h) "Got /help command"
   return . singleton . MessageResponse . textToMessage . cfgHelpText . hConfig $ h

respondRepeatCommnad :: Monad m => Handle m -> m [Response a] 
respondRepeatCommnad h = do
   Logger.logInfo (hLoggerHandle h) "Got /repeat command"
   repetitionCount        <- fmap stateRepetitionCount . hGetState $ h
   let question            = cfgRepeatText . hConfig $ h
   let title               = "Current repetition count: " .< repetitionCount <> "\n" <> question
   let maxRepetitionCount = cfgMaxRepetitionsCount . hConfig $ h
   let buttons             = [(n, SetRepetitionCountEvent n) | n <- [1..maxRepetitionCount]]
   return . singleton $ MenuResponse title buttons

respondOnMessage :: (Monad m, Message a) => Handle m -> a -> m [Response a]
respondOnMessage h msg = do
   repetitionCount <- fmap stateRepetitionCount . hGetState $ h
   Logger.logInfo (hLoggerHandle h) $
      "User sended message, echoing (" .< repetitionCount <>" times) : "
      <> (fromMaybe "<message can't be shown>" (messageToText msg))
   replicateM repetitionCount . return $ echo msg

echo :: Message a => a -> Response a
echo = MessageResponse