module Retuner (main) where

import Prelude
import Data.Int (toNumber)
import Data.Foldable (minimum, maximum)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import MaxForLive.Global (
    outlet
  , setInletAssist
  , setInlets
  , setOutletAssist
  , setOutlets
--  , postLn
  )
import MaxForLive.Handlers (CollectRemaining(..), setHandler)
import MaxForLive.Message (Message(..))

import Backend.Note (InOctave)

import Retuner.State (RetunerState)
import Retuner.State as State

main :: Effect Unit
main = do
    setInlets 2
    setOutlets 4

    setInletAssist 0 "Handpan tuning"
    setInletAssist 1 "Selected scale"

    setOutletAssist 0 "Scale size (to split)"
    setOutletAssist 1 "To funbuff"
    setOutletAssist 3 "To multislider"

    ref <- Ref.new State.initial

    setHandler { inlet: 0, msg: "list", handler: setTuning ref }
    setHandler { inlet: 1, msg: "list", handler: setScale  ref }

update :: Ref RetunerState -> (RetunerState -> RetunerState) -> Effect Unit
update ref f = do
    newState <- Ref.modify f ref

    -- Size of the scale (including the doum)
    -- This disables the "split" if the scale is empty.
    outlet 0 (List.length newState.scale - 1)

    unless (List.null newState.tuning) $ do
      let retunings = State.retunings newState

      -- Configure the LUT
      -- (First value in the "set" message is the starting index in the table)
      outlet 1 "clear"
      outlet 1 $ Message { name: "set", payload: (0 : retunings) }

      -- Configure the min/max displays
      case minimum retunings of
        Nothing -> pure unit -- Can't actually happen
        Just m  -> outlet 2 $ Message { name: "min", payload: m }
      case maximum retunings of
        Nothing -> pure unit -- Can't actually happen
        Just m  -> outlet 2 $ Message { name: "max", payload: m }

      -- Configure the multisliders
      outlet 3 $ Message { name: "size", payload: List.length newState.tuning }
      outlet 3 $ normalize retunings



setScale :: Ref RetunerState -> CollectRemaining List InOctave
setScale ref = CollectRemaining $ \scale -> update ref (_ { scale = scale })

setTuning :: Ref RetunerState -> CollectRemaining List InOctave
setTuning ref = CollectRemaining $ \tuning -> update ref (_ { tuning = tuning })

-- | Normalize the retunings for the multislideer
-- |
-- | The sliders take values between -1 and 1. Any retunings by more than 12
-- | semitones (an octave) we just set to 1.
normalize :: List Int -> List Number
normalize = map go
  where
    go :: Int -> Number
    go t = toNumber t / 12.0
