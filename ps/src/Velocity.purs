module Velocity (main) where

import Prelude

import Data.List ((..))
import Data.Traversable (for_)

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import MaxForLive.Global (
    outlet
  , setInlets
  , setOutlets
  )
import MaxForLive.Handlers (
    setHandler
  )

import Velocity.Curve (VelocityCurve)
import Velocity.Curve as Curve

{-------------------------------------------------------------------------------
  Patcher main
-------------------------------------------------------------------------------}

updateCurve ::
     forall a.
     Ref VelocityCurve
  -> (a -> VelocityCurve -> VelocityCurve)
  -> a -> Effect Unit
updateCurve ref upd a = do
    c <- Ref.modify (upd a) ref
    for_ (0 .. 127) $ \i ->
      outlet 0 [i, Curve.apply c i]

main :: Effect Unit
main = do
    setInlets 4
    setOutlets 1

    state <- Ref.new Curve.default

    setHandler { inlet: 0, msg: "msg_float", handler: updateCurve state Curve.setDrive }
    setHandler { inlet: 1, msg: "msg_float", handler: updateCurve state Curve.setComp  }
    setHandler { inlet: 2, msg: "msg_int",   handler: updateCurve state Curve.setOutLo }
    setHandler { inlet: 3, msg: "msg_int",   handler: updateCurve state Curve.setOutHi }

    pure unit
