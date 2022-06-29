module Message where

import Data.Text (Text)
import qualified Data.Text as T 

class Message a where
   messageToText :: a -> Maybe Text
   textToMessage :: Text -> a

{-
instance Message ByteString where
   messageToText =
-}