{-# LANGUAGE OverloadedStrings #-}

-- To-Do 
-- add loger
-- add Message TypeClass
-- add Multimedia?
module ChatBot where

import Data.Text (Text)
import qualified Data.Text as T 

import Message

data Handle = Handle
   { hConfig :: Config
   }

data Config = Config
   { cfgHelpText              :: Text
   , cfgRepeatText            :: Text
   , cfgMaxRepetitionsCount   :: RepetitionCount
   , cfgDefaultRepeatCount    :: RepetitionCount
   }

data Event a = SetRepeatitionCountEvent RepetitionCount
             | MessageEvent a

data Response a = MessageResponse a
                | MenuResponse Title [(RepetitionCount, Event a)]

type Title = Text

type RepetitionCount = Int

respond :: Message a => Handle -> Event a -> [Response a]
respond h (SetRepeatitionCountEvent msg) =

respond h (MessageEvent msg) 
   | isCommand msg "\help"   = respondHelpCommand h
   | isCommand msg "\repeat" = respondRepeatCommnad h
   | otherwise               = respondOnMessage h msg

isCommand :: Message a => a -> Text -> Bool
isCommand msg cmd = case messageToText msg of
   Nothing      -> False
   Just msgText -> msgText == cmd

-- singleton or []?
respondHelpCommnad :: Message a => Handle -> [Response a] 
respondHelpCommnad = singleton . MessageResponse . textToMessage . cfgHelpText . hConfig

respondRepeatCommnad :: Handle -> [Response a] 
respondRepeatCommnad h = let 
   repeatitionCount    = cfgRepeatCount . hConfig $ h
   question            = cfgRepeatText . hConfig $ h
   title               = "Current repetition count: " <> (T.Pack n) <> "/n" <> question
   maxRepeatitionCount = cfgMaxRepetitionsCount . hConfig $ h
   buttons             = [(n, SetRepeatitionCountEvent n) | n <- [1..maxRepeatitionCount]]
   in singleton $ MenuResponse title buttons

respondOnMessage :: Handle -> a -> [Response a]
respondOnMessage h msg = replicate (repeatCount h) . MessageResponse

{-
echo :: a -> Response a
echo = MessageResponse
-}