module Backend.Scale (
    Scale(..)
  , scaleNotes
    -- * Specification
  , ScaleSpec
  , defaultSpec
  , renderSpec
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.List (List, (:), fromFoldable)
import Data.Show.Generic (genericShow)

import MaxForLive.Conversions (class ToMax, class FromMax, class SimpleEnum)
import MaxForLive.Conversions as C

import Backend.Note (Note(..), InOctave(..))
import Backend.Note as N

{-------------------------------------------------------------------------------
  Scales
-------------------------------------------------------------------------------}

data Scale =
    Kurd9
  | Hijaz
  | Pelog
  | Integral
  | CelticMinor

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
scaleNotes :: Scale -> Array Note
scaleNotes Kurd9       = [ A , As , C  , D , E , F  , G  , A , C ]
scaleNotes Hijaz       = [ D , Ds , Fs , G , A , As , C  , D     ]
scaleNotes Pelog       = [ A , As , C  , E , F , A  , As , C     ]
scaleNotes Integral    = [ A , As , C  , D , E , F  , A          ]
scaleNotes CelticMinor = [ A , C  , D  , E , F , G  , A  , C     ]

{-------------------------------------------------------------------------------
  Specification of the scale of a handpan
-------------------------------------------------------------------------------}

type ScaleSpec = { scale :: Scale, doum :: InOctave, root :: InOctave }

-- | Default spec
--
-- These match the default in the patcher.
defaultSpec :: ScaleSpec
defaultSpec = {
      scale: Kurd9
    , doum: InOctave { octave: 3, note: C }
    , root: InOctave { octave: 3, note: C }
    }

-- | Rendering a spec
renderSpec :: ScaleSpec -> List InOctave
renderSpec { scale, doum, root } =
    doum : render (scaleNotes scale)
  where
    render :: Array Note -> List InOctave
    render =
            fromFoldable
        >>> map (\n -> InOctave { octave: 0, note: n })
        >>> N.monotonic
        >>> N.startWith root
