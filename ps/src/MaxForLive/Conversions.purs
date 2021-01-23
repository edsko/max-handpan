module MaxForLive.Conversions (
    MaxValue
  , class FromMaxValue
  , fromMaxValue
  , class ToMaxValue
  , toMaxValue
  ) where

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | Uninterpreted value in the Max world
foreign import data MaxValue :: Type

-- | Translate from the Max world to the PureScript world
class FromMaxValue a where
  fromMaxValue :: MaxValue -> a

-- | Translate from the PureScript world to the Max world
class ToMaxValue a where
  toMaxValue :: a -> MaxValue
