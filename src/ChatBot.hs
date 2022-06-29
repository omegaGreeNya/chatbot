{-# LANGUAGE OverloadedStrings #-}

-- To-Do 
-- add loger
-- add Message TypeClass
-- add Multimedia?
module ChatBot where

import Control.Monad (replicateM)
--import Data.List (singleton)
import Data.Text (Text)
import qualified Data.Text as T 

import Message

data Handle m = Handle
   { hConfig :: Config
   , hGetState :: m State
   , hModifyState :: (State -> State) -> m ()
   }

data Config = Config
   { cfgHelpText              :: Text
   , cfgRepeatText            :: Text
   , cfgMaxRepetitionsCount   :: RepetitionCount
   , cfgDefaultRepeatCount    :: RepetitionCount
   }
   
data State = State
   { stateRepetitionCount :: RepetitionCount
   }

-- /\ smart constructors? /\

data Event a = SetRepeatitionCountEvent RepetitionCount
             | MessageEvent a

data Response a = MessageResponse a
                | MenuResponse Title [(RepetitionCount, Event a)]

type Title = Text

type RepetitionCount = Int

respond :: (Monad m, Message a) => Handle m -> Event a -> m [Response a]
respond h (SetRepeatitionCountEvent n) = 
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
handleChangeRepetitionCount h n =
   hModifyState h (\s -> s{stateRepetitionCount = n})
   >> return []

-- singleton or []?
respondHelpCommand :: (Monad m, Message a) => Handle m -> m [Response a] 
respondHelpCommand = return . singleton . MessageResponse . textToMessage . cfgHelpText . hConfig

-- is it's okay to not specify message t-c?
respondRepeatCommnad :: Monad m => Handle m -> m [Response a] 
respondRepeatCommnad h = do
   repeatitionCount        <- fmap stateRepetitionCount . hGetState $ h
   let question            = cfgRepeatText . hConfig $ h
   let title               = "Current repetition count: " <> (T.pack . show $ repeatitionCount) <> "/n" <> question
   let maxRepeatitionCount = cfgMaxRepetitionsCount . hConfig $ h
   let buttons             = [(n, SetRepeatitionCountEvent n) | n <- [1..maxRepeatitionCount]]
   return . singleton $ MenuResponse title buttons

respondOnMessage :: Monad m => Handle m -> a -> m [Response a]
respondOnMessage h msg = do
   repeatitionCount <- fmap stateRepetitionCount . hGetState $ h
   replicateM repeatitionCount . return $ MessageResponse msg

{-
echo :: a -> Response a
echo = MessageResponse
-}

singleton :: a -> [a]
singleton x = [x]