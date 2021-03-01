module TrackSelected (main) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import MaxForLive.Global (
    outlet
  , setInlets
  , setInletAssist
  , setOutlets
  , setOutletAssist
  , postLn
  )
import MaxForLive.Handlers (
    setHandler
  )
import MaxForLive.LiveAPI (
    LiveAPI
  , Id
  , Device
  , deviceTrack
  , thisDevice
  , unquotedPath
  , liveSet
  , selectedTrack
  , view
  , objectType
  )
import MaxForLive.LiveAPI as LiveAPI
import MaxForLive.Message (
    Message(..)
  )

import TrackSelected.State(State(..), defaultState)

main :: Effect Unit
main = do
    setInlets 4
    setOutlets 2

    setInletAssist 0 "Bang to (re)initialise"
    setInletAssist 1 "Device enabled/disabled (from live.thisdevice)"
    setInletAssist 2 "ID of selected track"
    setInletAssist 3 "ID of device at our path"

    setOutletAssist 0 "'selected' or 'deselected'"
    setOutletAssist 1 "Device path (on init and when device moved)"

    stVar <- Ref.new defaultState

    setHandler { inlet: 0, msg: "bang",    handler: init          stVar }
    setHandler { inlet: 1, msg: "msg_int", handler: toggleEnabled stVar }
    setHandler { inlet: 2, msg: "id",      handler: setSelectedId stVar }
    setHandler { inlet: 3, msg: "id",      handler: setDeviceId   stVar }

init :: Ref State -> Effect Unit
init stVar = do
    us <- LiveAPI.withPath thisDevice

    postLn $ "us: " <> show (unquotedPath us) <> " (" <> show (objectType us) <> ", " <> show (LiveAPI.id us) <> ")"

    ourTrack <- LiveAPI.deviceTrack thisDevice
    postLn $ "ourTrack: " <> show (unquotedPath ourTrack) <> " (" <> show (objectType ourTrack) <> ")"

    selected <- LiveAPI.withPath (selectedTrack (view liveSet))
    postLn $ "selected: " <> show (unquotedPath selected) <> " (" <> show (objectType selected) <> ")"

    let newState :: State
        newState = State { ourId: Just (LiveAPI.id us) }

    postLn $ "newState: " <> show newState
    Ref.write newState stVar

    if LiveAPI.sameId (LiveAPI.id ourTrack) (LiveAPI.id selected)
      then outlet 0 "selected"
      else outlet 0 "deselected"

    outlet 1 $ Message {
          messageName: "path"
        , messagePayload: unquotedPath us
        }

toggleEnabled :: Ref State -> Int -> Effect Unit
toggleEnabled stVar enabled = postLn $ "toggleEnabled: " <> show enabled

setSelectedId :: Ref State -> Int -> Effect Unit
setSelectedId stVar selected = postLn $ "setSelectedId: " <> show selected

setDeviceId :: Ref State -> Id Device -> Effect Unit
setDeviceId stVar deviceId = do
    State oldState <- Ref.read stVar

    -- Check if the ID has actually changed.
    --
    -- The most important use case for this case distinction is when we /do/
    -- (re) initialise the device, because when we set up the `live.path`, we
    -- will immediately get a notification (of our own ID), so if we don't
    -- take that into account, the patcher would fall into an infinite loop.
    if oldState.ourId /= Just deviceId
      then init stVar
      else pure unit
