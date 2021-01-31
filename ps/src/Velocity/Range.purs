module Velocity.Range (
    Ranged(..)
    -- | Conversion
  , withValue
  , withPosition
  , from
    -- | Updates
  , scale
  , compress
  , expand
  ) where

import Prelude

import Data.Int (toNumber, round)
import Data.Ord (signum)
import Math (log, abs, pow)

-- | Explicitly mark a value with its range
type Ranged = {
    -- | Lower end of the range
    lo :: Number

    -- | Upper end of the range
  , hi :: Number

    -- | Value within the range
    -- |
    -- | Invariant: 'rangeLo <= rangeValue <= rangeHi'
  , x :: Number
  }

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | Construct a `Ranged` value
-- |
-- | Precondition of `withValue lo hi x`: `lo <= x <= hi`.
withValue :: Number -> Number -> Int -> Ranged
withValue lo hi x = { lo, hi, x: toNumber x }

-- | Construct a `Ranged` value from a position
-- |
-- | Precondition of `mk lo hi x`: `lo <= x <= hi`
-- | Postcondition: `position (withPosition lo hi p) == p`
withPosition :: Number -> Number -> Number -> Ranged
withPosition lo hi p = { lo, hi, x: lo + (hi - lo) * p }

from :: Ranged -> Int
from { x } = round x

-- | Width of the range
-- |
-- | `width r == rangeHi r - rangeLo r`
width :: Ranged -> Number
width { hi , lo } = hi - lo

-- | The relative position of the value within the range
--
-- Postconditions:
--
-- * `0 <= position r <= 1`.
-- * `(lo r) + (position r) * (width r) == value r`
position :: Ranged -> Number
position r@{ x, lo } = (x - lo) / width r

{-------------------------------------------------------------------------------
  Updates
-------------------------------------------------------------------------------}

-- | Set `lo` and `hi`, scaling the value accordingly.
--
-- When calling `scale lo hi`, `lo` and `hi` will be swapped if `lo > hi`.
scale :: Int -> Int -> Ranged -> Ranged
scale lo' hi' r = withPosition lo hi (position r)
  where
    {lo, hi} = if lo' <= hi'
                 then {lo: toNumber lo', hi: toNumber hi'}
                 else {lo: toNumber hi', hi: toNumber lo'}

-- | Apply m-law compression
-- |
-- | Precondition of `compress mu r`: `-1 <= lo r <= hi r <= +1'
compress :: Number -> Ranged -> Ranged
compress mu r = r {
      x = signum r.x * log (1.0 + mu * abs r.x) / log (1.0 + mu)
    }

-- | Apply m-law expansion
-- |
-- | Precondition of `expand mu r`: `-1 <= lo r <= hi r <= +1'
expand :: Number -> Ranged -> Ranged
expand mu r = r {
      x = signum r.x * (1.0 / mu) * (pow (1.0 + mu) (abs r.x) - 1.0)
    }
