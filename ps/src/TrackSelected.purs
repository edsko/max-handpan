module TrackSelected (main) where

import Prelude
import Effect (Effect)

import MaxForLive.Global (
    outlet
  , setInlets
  , setOutlets
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

main :: Effect Unit
main = do
    setInlets 1
    setOutlets 3

    setHandler { inlet: 0, msg: "bang", handler: init }

init :: Effect Unit
init = do
    us <- LiveAPI.withPath thisDevice

    postLn $ "us: " <> show (unquotedPath us) <> " (" <> show (objectType us) <> ")"

    ourTrack <- LiveAPI.deviceTrack thisDevice
    postLn $ "ourTrack: " <> show (unquotedPath ourTrack) <> " (" <> show (objectType ourTrack) <> ")"

    selected <- LiveAPI.withPath (selectedTrack (view liveSet))
    postLn $ "selected: " <> show (unquotedPath selected) <> " (" <> show (objectType selected) <> ")"

    if LiveAPI.sameId (LiveAPI.id ourTrack) (LiveAPI.id selected)
      then outlet 0 "selected"
      else outlet 0 "deselected"

    outlet 2 0 -- Disable notification from live.path (avoid infinite loop)
    outlet 1 $ Message {
          messageName: "path"
        , messagePayload: unquotedPath us
        }
    outlet 2 1 -- Re-enable notifications
