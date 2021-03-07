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

import TrackSelected.State (State)
import TrackSelected.State as State

main :: Effect Unit
main = do
    setInlets 6
    setOutlets 3

    setInletAssist 0 "Bang to (re)initialise"
    setInletAssist 1 "Device enabled/disabled (from live.thisdevice)"
    setInletAssist 2 "Bang when preview state disabled"
    setInletAssist 3 "Bang when preview state enabled"
    setInletAssist 4 "ID of selected track"
    setInletAssist 5 "ID of device at our path"

    setOutletAssist 0 "'selected' or 'deselected'"
    setOutletAssist 1 "Device path (on init and when device moved)"

    st <- Ref.new State.init

    setHandler { inlet: 0, msg: "bang",    handler: init           st }
    setHandler { inlet: 1, msg: "msg_int", handler: toggleEnabled  st }
    setHandler { inlet: 2, msg: "bang",    handler: previewDisable st }
    setHandler { inlet: 3, msg: "bang",    handler: previewEnable  st }
    setHandler { inlet: 4, msg: "id",      handler: setSelectedId  st }
    setHandler { inlet: 5, msg: "id",      handler: setDeviceId    st }

init :: Ref State -> Effect Unit
init ref = do
    us        <- LiveAPI.withPath    LiveAPI.thisDevice
    mOurTrack <- LiveAPI.deviceTrack LiveAPI.thisDevice

    case mOurTrack of
      Just ourTrack -> do
        updateState ref $ \st -> st {
            ourId   = Just (LiveAPI.id us)
          , parent  = Just (LiveAPI.id ourTrack)
          , preview = true
          }

        outlet 1 "Off"
        outlet 2 $ Message {
              messageName: "path"
            , messagePayload: LiveAPI.unquotedPath us
            }
      Nothing -> do
        outlet 1 "Error"
        updateState ref $ \st -> st {
           ourId   = Nothing
         , parent  = Nothing
         , preview = true
         }

updateState :: Ref State -> (State -> State) -> Effect Unit
updateState ref f = do
    oldState <- Ref.read ref
    let newState = f oldState
    Ref.write newState ref

    if State.isSelected oldState == State.isSelected newState
      then pure unit -- Nothing changed, don't output anything
      else outlet 0 (State.isSelected newState)

toggleEnabled :: Ref State -> Boolean -> Effect Unit
toggleEnabled ref enabled = do
    updateState ref (_ { enabled = enabled })

setSelectedId :: Ref State -> Id Track -> Effect Unit
setSelectedId ref selected = do
    updateState ref (_ { selected = Just selected })

previewDisable :: Ref State -> Effect Unit
previewDisable ref = do
    updateState ref (_ { preview = false })

previewEnable :: Ref State -> Effect Unit
previewEnable ref = do
    oldState <- Ref.read ref

    if not oldState.preview
      then init ref -- Reinitialize when preview is re-enabled
      else pure unit

setDeviceId :: Ref State -> Id Device -> Effect Unit
setDeviceId ref deviceId = do
    oldState <- Ref.read ref

    -- Check if the ID has actually changed.
    --
    -- The most important use case for this case distinction is when we /do/
    -- (re)initialise the device, because when we set up the `live.path`, we
    -- will immediately get a notification (of our own ID), so if we don't
    -- take that into account, the patcher would fall into an infinite loop.
    if oldState.ourId /= Just deviceId
      then init ref
      else pure unit
