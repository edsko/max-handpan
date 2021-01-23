module MaxForLive.Conversions (
    MaxValue
  , class FromMax
  , fromMax
  , class ToMax
  , toMax
  ) where

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
  Instances
-------------------------------------------------------------------------------}

foreign import fromMaxIntImpl :: MaxValue -> Int

instance fromMaxInt :: FromMax Int where
  fromMax = fromMaxIntImpl
