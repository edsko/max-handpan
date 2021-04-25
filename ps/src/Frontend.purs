module Frontend (main) where

import Prelude
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Effect (Effect)

import MaxForLive.Global (
    outlet
--  , postLn
  , setAutowatch
  , setInlets
  , setOutlets
  )
import MaxForLive.Handlers (setHandler)
import MaxForLive.Message (Message(..), Bang(..))
import MaxForLive.Push (Push, Button)
import MaxForLive.Push as Push

import Frontend.Layout (Layout(..), defaultLayout)
import Frontend.Colors (Colors(..), defaultColors)

main :: Effect Unit
main = do
    setInlets  1
    setOutlets 1

    -- Disable autowatch. For justification, see
    -- http://edsko.net/2020/12/27/trichords-part2/
    setAutowatch 0

    push <- Push.new

    setHandler { inlet: 0, msg: "setSelected", handler: setSelected push }
    setHandler { inlet: 0, msg: "init", handler: init push }

{-------------------------------------------------------------------------------
  Message handlers
-------------------------------------------------------------------------------}

init :: Push -> Effect Unit
init push = do
    -- We can't set these up in `main`, because the outlet of the JS object
    -- isn't available until the device is fully initialized.
    setupLUTs push

setSelected :: Push -> Boolean -> Effect Unit
setSelected push selected
  | selected  = activate   push
  | otherwise = deactivate push

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Configure the layout of the Push, and set the LUTs accordingly
setupLUTs :: Push -> Effect Unit
setupLUTs push = do
    -- Reset the LUTs

    outlet 0 $ Message { name: "reset", payload: Bang }

    -- Configure the various parts of the handpan

    --
    -- Melodic
    --

    -- Doum
    for_ layout.doum $ \b -> do
      push.setButtonMatrixColor b colors.doum
      outlet 0 $ Message { name: "setNote", payload: [buttonIx b, 48] }
      outlet 0 $ Message { name: "setVelocity", payload: [buttonIx b, 0] }

    -- Tone fields
    forWithIndex_ layout.tonefields $ \ix b -> do
      push.setButtonMatrixColor b colors.tonefield
      outlet 0 $ Message { name: "setNote", payload: [ buttonIx b, 49 + ix ] }
      outlet 0 $ Message { name: "setVelocity", payload: [ buttonIx b, 0] }

    --
    -- Ghost notes
    --

    -- TODO: Tak-as-ghost-note currently unassigned

    -- Ghost notes
    forWithIndex_ layout.ghostnotes $ \ix b -> do
      push.setButtonMatrixColor b colors.ghostnote
      outlet 0 $ Message { name: "setNote", payload: [ buttonIx b, 61 + ix ] }
      outlet 0 $ Message { name: "setVelocity", payload: [ buttonIx b, 1 ] }

    --
    -- Percussion
    --

    -- Bass (36)
    for_ layout.bass $ \b -> do
      push.setButtonMatrixColor b colors.bass
      outlet 0 $ Message { name: "setNote", payload: [buttonIx b, 36] }
      outlet 0 $ Message { name: "setVelocity", payload: [buttonIx b, 0] }

    -- Tak (37, 38)
    forWithIndex_ layout.taks $ \ix b -> do
      push.setButtonMatrixColor b colors.tak
      outlet 0 $ Message { name: "setNote", payload: [buttonIx b, 37 + ix] }
      outlet 0 $ Message { name: "setVelocity", payload: [buttonIx b, 0] }

    -- Slaps (39, 40)
    forWithIndex_ layout.slaps $ \ix b -> do
      push.setButtonMatrixColor b colors.slap
      outlet 0 $ Message { name: "setNote", payload: [buttonIx b, 39 + ix] }
      outlet 0 $ Message { name: "setVelocity", payload: [buttonIx b, 0] }

  where
    Layout layout = defaultLayout
    Colors colors = defaultColors

-- | Index of this button in the LUT
buttonIx :: Button -> Int
buttonIx button = button.col * 8 + button.row

-- | Invoked whenever the track is selected
activate :: Push -> Effect Unit
activate push = do
    push.grabButtonMatrix

    -- Provided we found the push, output the ID of the button matrix so that
    -- we can start to monitor it (routing the notifications from buttons
    -- pressed through the LUTs)
    push.withButtonMatrixId $ \matrixId ->
      outlet 0 $ Message { name: "buttonMatrixId", payload: matrixId }

-- | Invoked whenever the track is deselected
deactivate :: Push -> Effect Unit
deactivate push = do
    push.releaseButtonMatrix
