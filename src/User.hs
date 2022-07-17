-- | This model provides user abstraction,
-- and simpliest User data base with methods.
module User where
{-
import Data.Text (Text)
import Data.Map.Strict (Map, insertWith, 

-- | Defines from witch frontend this user is, 
-- and his id type (from that frontend).
data UserId a -- 'a' is phantom type, used to explicity track
   = TelegramUser Text
--   | VKUser Int
   deriving (Show, Ord)

data Telegram

data User = User
   { userId :: UserId a
   , userState :: IORef UserState
   }

data UserState = UserState
   { userStateRepetitionCount :: Int
   -- ^ How much echoed message user should receive
   } deriving (Show)

type UsersMap = Map UserID User

addTelegrammUser :: UsersMap 
                 -> UserID Telegram
                 -> 
addTelegrammUser 
-}