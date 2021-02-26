module Frontend (main) where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.List (List(..), (:), (..))
import MaxForLive.Global (
    outlet
  , setInlets
  , setOutlets
  , postLn
  )
import MaxForLive.Handlers (
    setHandler
  )

import MaxForLive.LiveAPI as LiveAPI
import MaxForLive.LiveAPI (
    LiveAPI
  , ControlSurface
  , controlSurface
  , countControlSurfaces
  , grabControl
  , id
  , liveApp
  , liveSet
  , objectType
  , releaseControl
  , sameId
  , selectedTrack
  , thisTrack
  , view
  )
import MaxForLive.Util (
    firstJustM
  )

main :: Effect Unit
main = do
    setInlets 1
    setOutlets 1

    setHandler { inlet: 0, msg: "grab", handler: handleGrab }
    setHandler { inlet: 0, msg: "release", handler: handleRelease }

withPush :: (LiveAPI ControlSurface -> Effect Unit) -> Effect Unit
withPush k = do
    liveApp <- LiveAPI.new liveApp
    numControlSurfaces <- countControlSurfaces liveApp
    postLn $ "numControlSurfaces: " <> show numControlSurfaces

    mPush <- firstJustM (0 .. (numControlSurfaces - 1)) $ \i -> do
      controlSurface <- LiveAPI.new (controlSurface i)
      if objectType controlSurface == "Push2"
        then pure (Just controlSurface)
        else pure Nothing
    case mPush of
      Nothing ->
        postLn "No push found"
      Just push ->
        k push

handleGrab :: Effect Unit
handleGrab = withPush $ \push ->
    -- TODO: We might want a more strongly typed layer around the Push
    grabControl push "Button_Matrix"

handleRelease :: Effect Unit
handleRelease = withPush $ \push ->
    releaseControl push "Button_Matrix"


{-
  if(control) {
    this.controller.call("grab_control", "Button_Matrix");
    var initColorsTask = new Task(initColors, this);
    initColorsTask.schedule(10);
  } else {
    this.controller.call("release_control", "Button_Matrix");
  }
-}


{-
    ourTrack <- LiveAPI.new thisTrack
    selected <- LiveAPI.new (selectedTrack (view liveSet))

    let ourTrackSelected = sameId (id ourTrack) (id selected)

    pure unit
-}

{-
    var ourTrack      = new LiveAPI(null, "this_device canonical_parent");
    var selectedTrack = new LiveAPI(null, "live_set view selected_track");
    var selected      = ourTrack.id == selectedTrack.id;
    var trackNo       = parseInt(ourTrack.unquotedpath.split(" ")[2]);

    if(this.selected != selected || this.trackNo != trackNo) {
      this.selected = selected;
      this.trackNo  = trackNo;
      callback.call(object, trackNo, selected);
    }
-}
