module Frontend (main) where

import Prelude
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Effect (Effect)

import MaxForLive.Global (
    setInlets
  , setOutlets
  , outlet
  )
import MaxForLive.Handlers (setHandler)
import MaxForLive.Message (Message(..), Bang(..))
import MaxForLive.Push (Push)
import MaxForLive.Push as Push

import Frontend.Layout (Layout(..), defaultLayout)
import Frontend.Colors (Colors(..), defaultColors)

main :: Effect Unit
main = do
    setInlets  1
    setOutlets 1

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

    outlet 0 $ Message {
        messageName: "reset"
      , messagePayload: Bang
      }

    -- Configure the various parts of the handpan

    forWithIndex_ layout.tonefields $ \ix button -> do
      push.setButtonMatrixColor button colors.tonefield
      outlet 0 $ Message {
          messageName: "setNote"
        , messagePayload: [ button.col * 8 + button.row , 48 + ix ]
        }
      outlet 0 $ Message {
          messageName: "setVelocity"
        , messagePayload: [ button.col * 8 + button.row , 1 ]
        }
    for_ layout.ghostnotes $ \button -> do
      push.setButtonMatrixColor button colors.ghostnote
    for_ layout.slaps $ \button -> do
      push.setButtonMatrixColor button colors.slap

    for_ layout.doum $ \button -> do
      push.setButtonMatrixColor button colors.doum
    for_ layout.tak $ \button -> do
      push.setButtonMatrixColor button colors.tak
    for_ layout.bass $ \button -> do
      push.setButtonMatrixColor button colors.bass
  where
    Layout layout = defaultLayout
    Colors colors = defaultColors

-- | Invoked whenever the track is selected
activate :: Push -> Effect Unit
activate push = do
    push.grabButtonMatrix

    -- Provided we found the push, output the ID of the button matrix so that
    -- we can start to monitor it (routing the notifications from buttons
    -- pressed through the LUTs)
    push.withButtonMatrixId $ \matrixId ->
      outlet 0 $ Message {
          messageName: "buttonMatrixId"
        , messagePayload: matrixId
        }

-- | Invoked whenever the track is deselected
deactivate :: Push -> Effect Unit
deactivate push = push.releaseButtonMatrix
