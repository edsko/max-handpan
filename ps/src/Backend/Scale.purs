module Backend.Scale (
    Scale(..)
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import MaxForLive.Conversions (class ToMax, class FromMax, class SimpleEnum)
import MaxForLive.Conversions as C

{-------------------------------------------------------------------------------
  Scales
-------------------------------------------------------------------------------}

data Scale =
    Kurd9
  | Hijaz  -- ^ also known as Hitzaz

derive instance genericScale :: Generic Scale _

instance showScale    :: Show    Scale where show    = genericShow
instance fromMaxScale :: FromMax Scale where fromMax = C.maxToEnum
instance toMaxScale   :: ToMax   Scale where toMax   = C.maxFromEnum

instance simpleEnumScale :: SimpleEnum Scale where
  toSimpleEnum   = C.genericToSimpleEnum
  fromSimpleEnum = C.genericFromSimpleEnum

{-------------------------------------------------------------------------------
  Notes in a scale
-------------------------------------------------------------------------------}

{-
with(exports.Note) {
  with(exports.Scale) {
    exports.scales[KURD_9] = [D, A, Bb, C,  D, E, F,  G, A, C];
    exports.scales[HIJAZ]  = [G, D, Eb, Fs, G, A, Bb, C, D];
  }
}
-}

--scaleNotes :: { scale :: Scale, doum :: Note, root :: Note } -> [InOctave]
--scaleNotes =
