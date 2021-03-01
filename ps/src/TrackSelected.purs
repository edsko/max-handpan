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
  )
import MaxForLive.Handlers (setHandler)
import MaxForLive.LiveAPI (Id, Device, Track)
import MaxForLive.LiveAPI as LiveAPI
import MaxForLive.Message (Message(..))

import TrackSelected.State(State(..))
import TrackSelected.State as State

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

    st <- Ref.new State.init

    setHandler { inlet: 0, msg: "bang",    handler: init          st }
    setHandler { inlet: 1, msg: "msg_int", handler: toggleEnabled st }
    setHandler { inlet: 2, msg: "id",      handler: setSelectedId st }
    setHandler { inlet: 3, msg: "id",      handler: setDeviceId   st }

init :: Ref State -> Effect Unit
init st = do
    us       <- LiveAPI.withPath    LiveAPI.thisDevice
    ourTrack <- LiveAPI.deviceTrack LiveAPI.thisDevice

    updateState st $ State.setDevice {
        ourId:  LiveAPI.id us
      , parent: LiveAPI.id ourTrack
      }

    outlet 1 $ Message {
          messageName: "path"
        , messagePayload: LiveAPI.unquotedPath us
        }

updateState :: Ref State -> (State -> State) -> Effect Unit
updateState st f = do
    oldState <- Ref.read st
    let newState = f oldState
    Ref.write newState st

    if State.isSelected oldState == State.isSelected newState then
      -- Nothing changed, don't output anything
      pure unit
    else
      if State.isSelected newState
        then outlet 0 "selected"
        else outlet 0 "deselected"

toggleEnabled :: Ref State -> Boolean -> Effect Unit
toggleEnabled st = updateState st <<< State.setEnabled

setSelectedId :: Ref State -> Id Track -> Effect Unit
setSelectedId st = updateState st <<< State.setSelected

setDeviceId :: Ref State -> Id Device -> Effect Unit
setDeviceId st deviceId = do
    State oldState <- Ref.read st

    -- Check if the ID has actually changed.
    --
    -- The most important use case for this case distinction is when we /do/
    -- (re)initialise the device, because when we set up the `live.path`, we
    -- will immediately get a notification (of our own ID), so if we don't
    -- take that into account, the patcher would fall into an infinite loop.
    if oldState.ourId /= Just deviceId
      then init st
      else pure unit
