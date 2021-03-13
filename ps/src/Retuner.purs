module Retuner (main) where

import Prelude
import Data.Int (toNumber)
import Data.List (List)
import Data.List as List
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import MaxForLive.Global (
    outlet
  , setInletAssist
  , setInlets
  , setOutletAssist
  , setOutlets
  , postLn
  )
import MaxForLive.Handlers (CollectRemaining(..), setHandler)
import MaxForLive.Message (Message(..))

import Backend.Note (InOctave)

import Retuner.State (RetunerState)
import Retuner.State as State

main :: Effect Unit
main = do
    setInlets 2
    setOutlets 1

    setInletAssist 0 "Scale"
    setInletAssist 1 "Tuning"

    setOutletAssist 0 "To multislider"

    ref <- Ref.new State.initial

    setHandler { inlet: 0, msg: "list", handler: setScale  ref }
    setHandler { inlet: 1, msg: "list", handler: setTuning ref }

update :: Ref RetunerState -> (RetunerState -> RetunerState) -> Effect Unit
update ref f = do
    newState <- Ref.modify f ref

    unless (List.null newState.tuning) $ do
      outlet 0 $ Message { name: "size", payload: List.length newState.tuning }
      postLn $ show (State.retunings newState)
      outlet 0 $ normalize (State.retunings newState)

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
