module Backend.Note (
    Note(..)
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import MaxForLive.Conversions (class ToMax, class FromMax, class SimpleEnum)
import MaxForLive.Conversions as C

{-------------------------------------------------------------------------------
  Notes
-------------------------------------------------------------------------------}

data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B

derive instance genericNote :: Generic Note _

instance showNote    :: Show    Note where show    = genericShow
instance fromMaxNote :: FromMax Note where fromMax = C.maxToEnum
instance toMaxNote   :: ToMax   Note where toMax   = C.maxFromEnum

instance simpleEnumNote :: SimpleEnum Note where
  toSimpleEnum   = C.genericToSimpleEnum
  fromSimpleEnum = C.genericFromSimpleEnum

{-------------------------------------------------------------------------------
  Octaves
-------------------------------------------------------------------------------}

newtype InOctave = InOctave { octave :: Int, note :: Note }

derive instance genericInOctave :: Generic InOctave _

instance showInOctave    :: Show    InOctave where show    = genericShow
instance fromMaxInOctave :: FromMax InOctave where fromMax = C.maxToEnum
instance toMaxInOctave   :: ToMax   InOctave where toMax   = C.maxFromEnum

instance simpleEnumInOctave :: SimpleEnum InOctave where
  fromSimpleEnum (InOctave { octave, note }) =
      octave * 12 + C.fromSimpleEnum note
  toSimpleEnum i =
      InOctave {
          octave: i `div` 12
        , note: C.toSimpleEnum (i `mod` 12)
        }
