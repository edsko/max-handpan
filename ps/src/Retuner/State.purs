module Retuner.State (
    RetunerState
  , initial
  ) where

import Data.List (List(..))

import Backend.Note (InOctave)

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
