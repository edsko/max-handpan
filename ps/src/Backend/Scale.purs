module Backend.Scale (
    Scale(..)
    -- * Specification
  , ScaleSpec
  , defaultSpec
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import MaxForLive.Conversions (class ToMax, class FromMax, class SimpleEnum)
import MaxForLive.Conversions as C

import Backend.Note (Note(..))

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

-- | Notes in the scale
--
-- The root note of the scale here doesn't matter, whatever is convenient.
-- We will anyway transpose it up or down as required.
--
-- The scale will automatically be repeated the next octave up if it must be
-- extended to more notes.
scaleNotes :: Scale -> Array Note
scaleNotes Kurd9 = [ A , As , C  , D , E , F  , G , A , C ]
scaleNotes Hijaz = [ D , Ds , Fs , G , A , As , C ]

{-------------------------------------------------------------------------------
  Specification of the scale of a handpan
-------------------------------------------------------------------------------}

type ScaleSpec = { scale :: Scale, doum :: Note, root :: Note }

-- | Default spec
--
-- These match the default in the patcher.
defaultSpec :: ScaleSpec
defaultSpec = { scale: Kurd9, doum: D, root: A }


{-
with(exports.Note) {
  with(exports.Scale) {
    exports.scales[KURD_9] = [D, A, Bb, C,  D, E, F,  G, A, C];
    exports.scales[HIJAZ]  = [G, D, Eb, Fs, G, A, Bb, C, D];
  }
-}

--scaleNotes :: { scale :: Scale, doum :: Note, root :: Note } -> [InOctave]
--scaleNotes =
