-- | Console application runner

-- To-Do
-- (???) We assume that each button is unique. Rework it maybe?
-- Vocabulary?
module FrontEnd.Console where
{-  ( Handle (..)
   , run
   ) where


import Data.List (find)
import Data.Text (Text)
import System.IO (BufferMode(..), hGetBuffering, hSetBuffering, stdout)
import Text.Read (readMaybe)
import qualified Data.Text.IO as TIO

import Lib
import qualified ChatBot

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
   bufMode <- hGetBuffering stdout
   
   -- Button output by lines is buffered w/o this command, and last line of buttons prints after choosing answer
   --    look evalResponse > putButtons and getButton
   -- God knows how much similar error-prone behavior might be with console, 
   --    so we enable NoBuffering during 'run' execution and disable it after.
   hSetBuffering stdout NoBuffering 
   
   TIO.putStrLn "Hey, im echo-bot. Type /help to get info about me."
   let loop = do
         msg <- TIO.getLine
         let event = parseEvent msg
         response <- ChatBot.respond (hBotHandle h) event
         evalResponse h response
         loop
   _ <- loop
   hSetBuffering stdout bufMode

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

putButtons :: [ChatBot.RepetitionCount] -> IO ()
putButtons buttons = putButtonsBy3 buttons 0

putButtonsBy3 :: [ChatBot.RepetitionCount] -> Int -> IO ()
putButtonsBy3 [] _ = TIO.putStrLn ""
putButtonsBy3 (x:xs) i = do
   TIO.putStr $ "[" .< x <> "] "
   case i of
      2 -> TIO.putStrLn "" >> putButtonsBy3 xs 0
      _ -> putButtonsBy3 xs (i + 1)

-- (???) We assume that each button is unique.
getButton :: [ChatBot.RepetitionCount] -> IO (Maybe (ChatBot.Event Text))
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
-}