-- | This module needed for precompiling template haskell parametrs (telegramDerivingOptions)
-- Due TH specific, we can't define options in same module there template is used.
module API.Telegram.DerivingExt
   (telegramDerivingDrop)
   where

import Data.Aeson.TH (Options (..), defaultOptions)

-- | Options for deriving To/FromJSON instances, due haskell template
-- specific, deriving represented in FrontEnd.Telegram.API.Types module
telegramDerivingOptions :: Options
telegramDerivingOptions = defaultOptions
   {omitNothingFields = True}

telegramDerivingDrop :: Int -> Options
telegramDerivingDrop n = telegramDerivingOptions {fieldLabelModifier = drop n}
