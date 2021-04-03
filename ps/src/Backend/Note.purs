module Backend.Note (
    -- * Note
    Note(..)
    -- * InOctave
  , InOctave(..)
  , transposeBy
  , distance
    -- * Rendering notes
  , monotonic
  , startWith
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Show.Generic (genericShow)

import MaxForLive.Conversions (class ToMax, class FromMax, class SimpleEnum)
import MaxForLive.Conversions as C

{-------------------------------------------------------------------------------
  Notes
-------------------------------------------------------------------------------}

data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B

derive instance genericNote :: Generic Note _
derive instance eqNote  :: Eq  Note
derive instance ordNote :: Ord Note

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

derive instance eqInOctave  :: Eq  InOctave
derive instance ordInOcatve :: Ord InOctave

instance simpleEnumInOctave :: SimpleEnum InOctave where
  fromSimpleEnum (InOctave { octave, note }) =
      octave * 12 + C.fromSimpleEnum note
  toSimpleEnum i =
      InOctave {
          octave: i `div` 12
        , note: C.toSimpleEnum (i `mod` 12)
        }

transposeBy :: Int -> InOctave -> InOctave
transposeBy n = C.fromSimpleEnum >>> (_ + n) >>> C.toSimpleEnum

-- | Distance between two notes in semitones
-- |
-- | Positive if first note is higher than the second, negative otherwise.
distance :: InOctave -> InOctave -> Int
distance a b = C.fromSimpleEnum a - C.fromSimpleEnum b

{-------------------------------------------------------------------------------
  Rendering notes
-------------------------------------------------------------------------------}

-- | Ensure a sequence of notes is monotonically increasing
--
-- At every point in the input list where monotonicity is violated, we go
-- up an octave.
monotonic :: List InOctave -> List InOctave
monotonic = go (-1)
  where
    go :: Int -> List InOctave -> List InOctave
    go _    Nil      = Nil
    go prev (x : xs) =
        if n > prev then
          x : go n xs
        else
          let t = (1 + (prev - n) / 12) * 12
          in transposeBy t x : go (n + t) xs
      where
        n = C.fromSimpleEnum x

-- | Transpose the list of notes so that it starts with the given note
startWith :: InOctave -> List InOctave -> List InOctave
startWith _  Nil     = Nil
startWith x (y : ys) = map (transposeBy (distance x y)) (y : ys)
