module Retuner (main) where

import Prelude
import Data.List (List)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import MaxForLive.Global (
    setInletAssist
  , setInlets
  , setOutletAssist
  , setOutlets
  , postLn
  )
import MaxForLive.Handlers (CollectRemaining(..), setHandler)

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
    postLn $ show newState

setScale :: Ref RetunerState -> CollectRemaining List InOctave
setScale ref = CollectRemaining $ \scale -> update ref (_ { scale = scale })

setTuning :: Ref RetunerState -> CollectRemaining List InOctave
setTuning ref = CollectRemaining $ \tuning -> update ref (_ { tuning = tuning })
