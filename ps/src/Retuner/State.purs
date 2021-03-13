module Retuner.State (
    RetunerState
  , initial
  , retunings
  ) where

import Prelude
import Data.List (List(..), (:))

import Backend.Note (InOctave, distance)

type RetunerState = {
      -- | Tuning of the handpan
      tuning :: List InOctave

      -- | Selected scale
    , scale :: List InOctave
    }

initial :: RetunerState
initial = {
      tuning: Nil
    , scale: Nil
    }

-- | Retuning of the handpan
-- |
-- | Positive values indicate that the required note is _higher_ than the note
-- | provided by the handpan; negative values indicate the required note is
-- | lower.
retunings :: RetunerState -> List Int
retunings { tuning, scale } = go tuning scale
  where
    go :: List InOctave -> List InOctave -> List Int
    go Nil      Nil      = Nil
    go (x : xs) (y : ys) = distance y x : go xs ys
    go (x : xs) Nil      = ignoreUnused (x : xs)
    go Nil      (y : ys) = reuseLast    (y : ys)

    -- If the scale has no more notes, some of the notes of the handpan may
    -- just sit unused. This isn't a big deal, and we just the retuning for
    -- those notes to 0.
    ignoreUnused :: List InOctave -> List Int
    ignoreUnused = map (const 0)

    -- If however the scale has more notes, but the handpan has run out,
    -- we have no choice but to reuse the last (highest) tone field.
    -- If there are no tone fields at all, we return the empty list
    -- (this can only happen during initialization)
    reuseLast :: List InOctave -> List Int
    reuseLast ys = findLast tuning
      where
        findLast :: List InOctave -> List Int
        findLast Nil       = Nil
        findLast (x : Nil) = map (\y -> distance y x) ys
        findLast (_ : xs)  = findLast xs
