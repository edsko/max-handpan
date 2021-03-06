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
--  , postLn
  )
import MaxForLive.Handlers (setHandler)

import Backend.Note (InOctave)
import Backend.Scale (Scale, ScaleSpec)
import Backend.Scale as Scale

main :: Effect Unit
main = do
    setInlets  3
    setOutlets 1

    setInletAssist 0 "Scale"
    setInletAssist 1 "Doum"
    setInletAssist 2 "Root"

    ref <- Ref.new Scale.defaultSpec

    setHandler { inlet: 0, msg: "msg_int", handler: setScale ref }
    setHandler { inlet: 1, msg: "msg_int", handler: setDoum  ref }
    setHandler { inlet: 2, msg: "msg_int", handler: setRoot  ref }

update :: Ref ScaleSpec -> (ScaleSpec -> ScaleSpec) -> Effect Unit
update ref f = do
    newSpec <- Ref.modify f ref
    outlet 0 (Scale.renderSpec newSpec)

setScale :: Ref ScaleSpec -> Scale -> Effect Unit
setScale ref scale = update ref (_ { scale = scale })

setRoot :: Ref ScaleSpec -> InOctave -> Effect Unit
setRoot ref root = update ref (_ { root = root })

setDoum :: Ref ScaleSpec -> InOctave -> Effect Unit
setDoum ref doum = update ref (_ { doum = doum })
