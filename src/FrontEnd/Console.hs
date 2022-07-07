-- | Console application runner

-- To-Do
-- (???) We assume that each button is unique. Rework it maybe?
-- Vocabulary?
module FrontEnd.Console
   ( Handle (..)
   , run
   ) where


import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Message
import Lib
import qualified ChatBot

instance Message Text where
   messageToText = Just
   textToMessage = id

data Handle = Handle
   { hBotHandle :: ChatBot.Handle IO
   --, hVocabulary :: Vocabulary
   }

{- Add?
data Vocabulary = Vocabulary
   { vHello :: Text
   , vWrongNumberPressed :: Text
   , vOutOfRangeNumberPressed :: Text
   ...
   }
-}


run :: Handle -> IO ()
run h = do
   TIO.putStrLn "Hey, im echo-bot. Type /help to get info about me."
   let loop = do
         msg <- TIO.getLine
         let event = parseEvent msg
         response <- ChatBot.respond (hBotHandle h) event
         evalResponse h response
         loop
   loop

parseEvent :: Text -> ChatBot.Event Text
parseEvent = ChatBot.MessageEvent 

evalResponse :: Handle -> [ChatBot.Response Text] -> IO ()
evalResponse _ [] =
   return ()
evalResponse h ((ChatBot.MessageResponse msg):responses) = do
   TIO.putStrLn msg
   evalResponse h responses
evalResponse h ((ChatBot.MenuResponse title buttons):responses) = do
   TIO.putStrLn title
   putButtons buttons
   mButtonEvent <- getButton buttons
   case mButtonEvent of
      Just event -> do
         response <- ChatBot.respond (hBotHandle h) event
         evalResponse h (response <> responses)
      Nothing ->
         evalResponse h responses

putButtons :: [(ChatBot.RepetitionCount, ChatBot.Event Text)] -> IO ()
putButtons buttons = putButtonsBy3 buttons 0

putButtonsBy3 :: [(ChatBot.RepetitionCount, ChatBot.Event Text)] -> Int -> IO ()
putButtonsBy3 [] _ = return ()
putButtonsBy3 ((x,_):xs) i = do
   TIO.putStr $ "[" .< x <> "] "
   when (i == 2) $ TIO.putStrLn ""
   putButtonsBy3 xs (i + 1)

-- (???) We assume that each button is unique.
getButton :: [(ChatBot.RepetitionCount, ChatBot.Event Text)] -> IO (Maybe (ChatBot.Event Text))
getButton buttons = do
   input <- getLine
   case readMaybe input of
      Nothing -> do
         TIO.putStrLn "Try to press number next time."
         return Nothing
      Just n -> do
         findButtonEvent buttons n

findButtonEvent :: [(ChatBot.RepetitionCount, ChatBot.Event Text)]
                -> ChatBot.RepetitionCount
                -> IO (Maybe (ChatBot.Event Text))
findButtonEvent buttons n = do
   let mEvent = find (\(m,_) -> m == n) buttons
   case mEvent of
      Nothing -> do
         TIO.putStrLn "You pressed not listed number! Fool!"
         return Nothing
      Just (_, event) -> return $ Just event