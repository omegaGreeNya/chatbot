-- | Type-class to convert between Text and Message-formats

module Message where

import           Data.Text (Text)
import qualified Data.Text as T

class Message a where
   messageToText :: a -> Maybe Text
   textToMessage :: Text -> a

instance Message Text where
   messageToText = Just
   textToMessage = id
