module Backend.Scale (
    Scale(..)
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Enum.Generic (genericToEnum, genericFromEnum)
import Data.Show.Generic (genericShow)

import MaxForLive.Conversions (
    class FromMax
  , class ToMax
  , fromMax
  , maxFromJust
  , toMax
  )

{-------------------------------------------------------------------------------
  Scales
-------------------------------------------------------------------------------}

data Scale =
    Kurd9
  | Hijaz  -- ^ also known as Hitzaz

derive instance genericScale :: Generic Scale _

instance showScale :: Show Scale where
  show = genericShow

instance fromMaxScale :: FromMax Scale where
  fromMax = maxFromJust <<< genericToEnum <<< fromMax

instance toMaxScale :: ToMax Scale where
  toMax = toMax <<< genericFromEnum
