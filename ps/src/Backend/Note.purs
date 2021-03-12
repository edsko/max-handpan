module Backend.Note (
    Note(..)
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
  Notes
-------------------------------------------------------------------------------}

data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B

derive instance genericNote :: Generic Note _

instance showNote :: Show Note where
  show = genericShow

instance fromMaxNote :: FromMax Note where
  fromMax = maxFromJust <<< genericToEnum <<< fromMax

instance toMaxNote :: ToMax Note where
  toMax = toMax <<< genericFromEnum
