-- | Utilities functions

-- To-Do
-- Organize all around
module Lib
   ( writeFileLog
   , readFileLog
   , (.<~)
   , (~>.)
   , (.<)
   , (>.)
   , renameFile
   , isPathSeparator
   , splitFileName
   ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (try, SomeException)
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Logger

-- <<< Safe version of "writeFile" with logging. Logs error before returning.
writeFileLog :: (MonadIO m, Show a) 
             => (FilePath -> a -> IO ())    -- ^ Function that saves data
             -> Logger.Handle m             -- ^ Logger handle
             -> Maybe Text                  -- ^ Text to log (loging Level INFO)
             -> FilePath                    -- ^ Target file location
             -> a                           -- ^ Data to save
             -> m (Either SomeException ()) -- ^ Error logged before returning
writeFileLog saveF h logMessage path dataToSave = do
   Logger.logInfo h $
      maybe ("Saving " .<~ path) id logMessage
   result <- liftIO . try $ saveF path dataToSave
   when (isLeft result) $
      Logger.logError h
         $ "Error during saving " 
         .<~ path
         <> " (file not saved): "
         .<~ (unsafeFromLeft result) -- since we know that result = Left _ is save to aplly unsafe unboxing
   return result
-- >>>

-- <<< Safe version of "readFile" with logging. Logs error before returning.
readFileLog :: (MonadIO m, Show a) 
            => (FilePath -> IO a)         -- ^ Function that reads data
            -> Logger.Handle m            -- ^ Logger handle
            -> Maybe Text                 -- ^ Text to log (loging Level INFO)
            -> FilePath                   -- ^ Target file location
            -> m (Either SomeException a) -- ^ Error logged before returning
readFileLog readF h logMessage path = do
   Logger.logInfo h $
      maybe ("Loading " .<~ path) id logMessage
   result <- liftIO . try $ readF path
   when (isLeft result) $
      Logger.logError h
         $ "Error during loading " 
         .<~ path
         <> " (file not loaded): "
         .<~ (unsafeFromLeft result) -- since we know that result = Left _ is save to aplly unsafe unboxing
   return result
-- >>>

-- <<<< Formating
(.<~) :: Show a => Text -> a -> Text
(.<~) text a = text <> "\"" <> (T.pack $ show a) <> "\""
infixr 7 .<~ 
(~>.) :: Show a => a -> Text -> Text
(~>.) = flip (.<~)
infixr 7 ~>.
(.<) :: Show a => Text -> a -> Text
(.<) text a = text <> (T.pack $ show a)
infixr 7 .<
(>.) :: Show a => a -> Text -> Text
(>.) = flip (.<)
infixr 7 >.
-- <> has precedence infixr 6. 
-- And we define infixr 7 for [.<, ..] since we want replace them to <>, and then concatinate
-- >>>>

-- <<<< FilePath manipulations
-- Takes "../example/path/file.name", "new_file.name" and returns "../example/path/new_file.name"
renameFile :: FilePath -> String -> FilePath
renameFile path name = locPath <> name
   where
      (locPath,_) = splitFileName path

splitFileName :: FilePath -> (String, String)
splitFileName = (\(xs, ys) -> (reverse xs, reverse ys)) . span (not . isPathSeparator) . reverse

isPathSeparator :: Char -> Bool
isPathSeparator '/'  = True
isPathSeparator '\\' = True
isPathSeparator _    = False


unsafeFromLeft :: Either a b -> a
unsafeFromLeft (Left a) = a
-- >>>>