module InspectSimpler (main) where

import Prelude

import Effect (Effect)

import MaxForLive.LiveAPI as LiveAPI
import MaxForLive.LiveAPI (
    id
  , liveSet
  , sameId
  , selectedTrack
  , thisTrack
  , view
  )

main :: Effect Unit
main = do
    ourTrack <- LiveAPI.new thisTrack
    selected <- LiveAPI.new (selectedTrack (view liveSet))

    let ourTrackSelected = sameId (id ourTrack) (id selected)

    pure unit

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
