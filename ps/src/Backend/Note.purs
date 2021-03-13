module Backend.Note (
    -- * Note
    Note(..)
    -- * InOctave
  , InOctave(..)
  , transposeBy
  , distance
    -- * Rendered
  , Rendered
  , renderedToList
  , render
  , renderOne
  , transposeTo
  ) where

import Prelude
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:), fromFoldable)
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

transposeBy :: Int -> InOctave -> InOctave
transposeBy n = C.fromSimpleEnum >>> (_ + n) >>> C.toSimpleEnum

-- | Distance between two notes in semitones
-- |
-- | Positive if first note is higher than the second, negative otherwise.
distance :: InOctave -> InOctave -> Int
distance a b = C.fromSimpleEnum a - C.fromSimpleEnum b

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

-- | A rendered scale is a series of notes laid across octaves, such that
-- the series of notes is monotonically increasing.
newtype Rendered = Rendered (List InOctave)

instance showRendered :: Show Rendered where
  show (Rendered notes) = show notes

instance monoidRendered :: Monoid Rendered where
  mempty = Rendered Nil

instance semigroupRendered :: Semigroup Rendered where
  append a b = appendMany (a : b : Nil)

instance toMaxRendered :: ToMax Rendered where
  toMax = renderedToList >>> C.toMax

{-------------------------------------------------------------------------------
  Working with 'Rendered'
-------------------------------------------------------------------------------}

renderedToList :: Rendered -> List InOctave
renderedToList (Rendered notes) = notes

appendMany :: List Rendered -> Rendered
appendMany = map renderedToList >>> go (-1) >>> Rendered
  where
    go :: Int -> List (List InOctave) -> List InOctave
    go _     Nil             = Nil
    go prev (Nil      : xss) = go prev xss
    go prev ((x : xs) : xss) =
        if n > prev then
          x : go n (xs : xss)
        else
          let t = (1 + (prev - n) / 12) * 12
          in transposeBy t x : go (n + t) (xs : xss)
      where
        n = C.fromSimpleEnum x

renderOne :: Note -> Rendered
renderOne note = Rendered (InOctave { octave: 0, note: note } : Nil)

render :: forall f. Foldable f => f Note -> Rendered
render = fromFoldable >>> map renderOne >>> appendMany

transposeTo :: InOctave -> Rendered -> Rendered
transposeTo x (Rendered rendered) = Rendered $
    case rendered of
      Nil  -> Nil
      y:ys -> map (transposeBy (distance x y)) (y : ys)
