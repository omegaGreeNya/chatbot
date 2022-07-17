-- | This module needed for precompiling template haskell parametrs (telegramDerivingOptions)
-- Due TH specific, we can't define options in same module there template is used.
module FrontEnd.Telegram.API.DerivingExt
   (telegramDerivingOptions)
   where

import Data.Aeson.TH (Options (..), defaultOptions)

-- | Options for deriving To/FromJSON instances, due haskell template
-- specific, deriving represented in FrontEnd.Telegram.API.Types module
telegramDerivingOptions :: Options
telegramDerivingOptions = defaultOptions
   { fieldLabelModifier = drop 1
   , omitNothingFields = True
   }