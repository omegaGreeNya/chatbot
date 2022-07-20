-- | Message it's type-class to convert between Text and Message-formats
module Message where

import Data.Text (Text)


-- | Type class to make log massages more precise.
class Message a where
   messageToText :: a -> Maybe Text
   -- ^ Gets message text to be printed.
   textToMessage :: Text -> a
   -- ^ Construct message with provided text.
   modifyMessage :: a -> a
   -- ^ Modifies text inside message (Not used currenly)
   -- Possible use case: making ver-very swedish accent for bot.
   -- Like that: https://www.youtube.com/watch?v=RqvCNb7fKsg
   -- (Half-Joke, dont waste time, or waste and pretend that you actually watching smthing useful)
   

instance Message Text where
   messageToText = Just
   textToMessage = id
   modifyMessage = id
