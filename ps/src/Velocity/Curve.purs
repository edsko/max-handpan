module Velocity.Curve (
    -- | Main type
    VelocityCurve
  , default
    -- | Setters
  , setDrive
  , setComp
  , setOutLo
  , setOutHi
    -- | Queries
  , apply
  ) where

import Prelude

import Velocity.Range (Ranged)
import Velocity.Range as Range

import Math (pow, abs)

{-------------------------------------------------------------------------------
  Main type
-------------------------------------------------------------------------------}

type VelocityCurve = {
    drive :: Number
  , comp  :: Number
  , outLo :: Int
  , outHi :: Int
  }

default :: VelocityCurve
default = {
      drive: 0.0
    , comp:  0.0
    , outLo: 0
    , outHi: 127
    }

{-------------------------------------------------------------------------------
  Setters
-------------------------------------------------------------------------------}

setDrive :: Number -> VelocityCurve -> VelocityCurve
setDrive n c = c { drive = n }

setComp :: Number -> VelocityCurve -> VelocityCurve
setComp n c = c { comp = n }

setOutLo :: Int -> VelocityCurve -> VelocityCurve
setOutLo n c = c { outLo = n }

setOutHi :: Int -> VelocityCurve -> VelocityCurve
setOutHi n c = c { outHi = n }

{-------------------------------------------------------------------------------
  Interpretation
-------------------------------------------------------------------------------}

apply :: VelocityCurve -> Int -> Int
apply { drive, comp, outLo, outHi } = leaveNoteOff $
        Range.withValue 0.0 127.0
        -- Apply drive (we use the positive part of the compansion curve)
    >>> Range.scale 0 1
    >>> compand drive
        -- Apply compansion
    >>> Range.scale (-1) 1
    >>> compand comp
        -- Rescale to user specified bounds
    >>> Range.scale outLo outHi
    >>> Range.from

-- | We want to leave velocity 0 (aka "note off") always at zero
leaveNoteOff :: (Int -> Int) -> Int -> Int
leaveNoteOff _ 0 = 0
leaveNoteOff f x = f x

-- | Apply compression/expansion parameter
-- |
-- | See http://edsko.net/2021/01/03/velocity-curve/
compand :: Number -> Ranged -> Ranged
compand comp =
    case comp of
      _ | comp > 0.0 -> Range.compress (mu comp)
      _ | comp < 0.0 -> Range.expand   (mu comp)
      _ | otherwise  -> identity
  where
    mu :: Number -> Number
    mu f = pow 10.0 (2.0 * abs f)
