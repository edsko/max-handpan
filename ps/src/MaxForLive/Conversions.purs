module MaxForLive.Conversions (
    MaxValue
  , class FromMax
  , fromMax
  , class ToMax
  , toMax
  ) where

import Unsafe.Coerce (unsafeCoerce)

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | Uninterpreted value in the Max world
foreign import data MaxValue :: Type

-- | Translate from the Max world to the PureScript world
class FromMax a where
  fromMax :: MaxValue -> a

-- | Translate from the PureScript world to the Max world
class ToMax a where
  toMax :: a -> MaxValue

{-------------------------------------------------------------------------------
  `FromMax` instances
-------------------------------------------------------------------------------}

foreign import fromMaxIntImpl    :: MaxValue -> Int
foreign import fromMaxStringImpl :: MaxValue -> String

instance fromMaxInt :: FromMax Int where
  fromMax = fromMaxIntImpl

instance fromMaxString :: FromMax String where
  fromMax = fromMaxStringImpl

{-------------------------------------------------------------------------------
  `ToMax` instances
-------------------------------------------------------------------------------}

instance toMaxInt :: ToMax Int where
  toMax = unsafeCoerce

instance toMaxString :: ToMax String where
  toMax = unsafeCoerce
