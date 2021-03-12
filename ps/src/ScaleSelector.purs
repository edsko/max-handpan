module ScaleSelector (main) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import MaxForLive.Global (
    setInletAssist
  , setInlets
  , setOutlets
  , outlet
--    postLn
  )
import MaxForLive.Handlers (setHandler)

import Backend.Note (Note)
import Backend.Scale (Scale, ScaleSpec)
import Backend.Scale as Scale

main :: Effect Unit
main = do
    setInlets  3
    setOutlets 1

    setInletAssist 0 "Scale"
    setInletAssist 1 "Root"
    setInletAssist 2 "Doum"

    ref <- Ref.new Scale.defaultSpec

    setHandler { inlet: 0, msg: "msg_int", handler: setScale ref }
    setHandler { inlet: 1, msg: "msg_int", handler: setDoum  ref }
    setHandler { inlet: 2, msg: "msg_int", handler: setRoot  ref }

updateSpec :: Ref ScaleSpec -> (ScaleSpec -> ScaleSpec) -> Effect Unit
updateSpec ref f = do
    newSpec <- Ref.modify f ref
    outlet 0 (Scale.renderSpec newSpec)

setScale :: Ref ScaleSpec -> Scale -> Effect Unit
setScale ref scale = updateSpec ref (_ { scale = scale })

setRoot :: Ref ScaleSpec -> Note -> Effect Unit
setRoot ref root = updateSpec ref (_ { root = root })

setDoum :: Ref ScaleSpec -> Note -> Effect Unit
setDoum ref doum = updateSpec ref (_ { doum = doum })
