-- | Access to the patcher itself
-- |
-- | See https://docs.cycling74.com/max8/vignettes/jspatcherobject
-- |
-- | This module is intended for qualified import.
module MaxForLive.Patcher (
    Patcher
  , patcher
  , filepath
  ) where

-- | The type of the patcher
foreign import data Patcher :: Type

-- | Our own patcher
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsglobal#patcher
foreign import patcher :: Patcher

-- | The patcher’s file path on disk
foreign import filepath :: Patcher -> String
