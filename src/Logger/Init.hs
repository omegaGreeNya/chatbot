-- | Logger Initialization interface.
module Logger.Init
   ( Handle 
   , Config (..)
   , createHandleIO
   , withHandle
   ) where

import qualified Data.Text.IO as TIO
import qualified System.IO (Handle)

import Logger.Impl (Handle (..), Config (..), withHandle)

-- | Handle constructor. Uses file handle.
createHandleIO :: System.IO.Handle -> Config -> Handle IO
createHandleIO handle cfg = Handle logger cfg
   where
      logger = TIO.hPutStrLn handle