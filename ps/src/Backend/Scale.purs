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
import Data.Show.Generic (genericShow)

import MaxForLive.Conversions (class ToMax, class FromMax, class SimpleEnum)
import MaxForLive.Conversions as C

import Backend.Note (Note(..), Rendered)
import Backend.Note as N

{-------------------------------------------------------------------------------
  Scales
-------------------------------------------------------------------------------}

data Scale =
    -- | Kurd 9
    -- |
    -- | Arabic scale. One of the most popular handpan scales.
    -- |
    -- | See https://www.haganenote.com/store/kurd/
    Kurd9

    -- | Hijaz
    -- |
    -- | Also known as:
    -- | * Phrygian dominant scale.
    -- | * Hitzaz
    -- | * Hijaz-Nahawand
    -- | * Hijaz maqam
    -- |
    -- | Fifth mode of the harmonic minor scale.
    -- | See https://en.wikipedia.org/wiki/Phrygian_dominant_scale
  | Hijaz  -- ^ Also known as

    -- | Pelog
    -- |
    -- | Also known as:
    -- | * Melog
    -- | * Melog/Selisir
    -- |
    -- | Indonesian pentatonic scale. Selisir is a _mode_ of this scale.
    -- | See https://en.wikipedia.org/wiki/Pelog
  | Pelog

    -- | Integral minor scale
    -- |
    -- | Mode of the natural minor scale (starting on the 5th degree) and
    -- | omitting the 4th degree, ending up with a hexatonic scale.
    -- | (Doum then normally a fifth below the root.)
    -- |
    -- | See https://www.sarazhandpans.com/handpan-scales/integral/
  | Integral

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
scaleNotes Kurd9    = [ A , As , C  , D , E , F  , G  , A , C ]
scaleNotes Hijaz    = [ D , Ds , Fs , G , A , As , C  , D     ]
scaleNotes Pelog    = [ A , As , C  , E , F , A  , As , C     ]
scaleNotes Integral = [ A , As , C  , D , E , F  , A          ]

{-------------------------------------------------------------------------------
  Specification of the scale of a handpan
-------------------------------------------------------------------------------}

type ScaleSpec = { scale :: Scale, doum :: Note, root :: Note }

-- | Default spec
--
-- These match the default in the patcher.
defaultSpec :: ScaleSpec
defaultSpec = { scale: Kurd9, doum: D, root: A }

-- | Rendering a spec
renderSpec :: ScaleSpec -> Rendered
renderSpec { scale, doum, root } =
       N.renderOne doum
    <> N.transposeTo root (N.render (scaleNotes scale))
