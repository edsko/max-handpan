-- | Set up Max message handlers
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsbasic
module MaxForLive.Handlers (
     -- | Max message handlers
     Handler -- opaque
   , mkHandler
   , setHandlers
     -- | FFI boundary
   , class InvokeHandler
   , invokeHandler
   ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (
    EffectFn1
  , mkEffectFn1
  , runEffectFn1
  )

import MaxForLive.Arguments (Arguments, getArg)
import MaxForLive.Conversions (class FromMax)

foreign import setHandlersImpl ::
     EffectFn1
       (Array { message :: String, handler :: EffectFn1 Arguments Unit })
       Unit

{-------------------------------------------------------------------------------
  Max message handlers
-------------------------------------------------------------------------------}

-- | Max message handler
-- |
-- | See `setHandlers` and `mkHandler`.
data Handler = MkHandler (forall r. (forall h. InvokeHandler h => h -> r) -> r)

-- | Construct `Handler`
mkHandler :: forall a.
     InvokeHandler a
  => String
  -> a
  -> { message :: String, handler :: Handler }
mkHandler message h = { message, handler: MkHandler (\k -> k h) }

-- | Set Max message handlers
-- |
-- | For example, to respond (only) to a bang:
-- |
-- | ```purescript
-- | setHandlers [mkHandler "bang" (postLn "BANG!")]
-- | ```
setHandlers ::
     Array { message :: String, handler :: Handler }
  -> Effect Unit
setHandlers = runEffectFn1 setHandlersImpl <<< map invokeHandler'

{-------------------------------------------------------------------------------
  FFI: Construct the untyped/typed boundary with Max
-------------------------------------------------------------------------------}

invokeHandler' :: forall  r.
     { handler :: Handler                  | r }
  -> { handler :: EffectFn1 Arguments Unit | r }
invokeHandler' r@{ handler: MkHandler someHandler } =
    someHandler (\h -> r{ handler = mkEffectFn1 (invokeHandler 0 h) })

{-------------------------------------------------------------------------------
  FFI: Functions of arbitrary arguments
-------------------------------------------------------------------------------}

class InvokeHandler a where
  invokeHandler :: Int -> a -> Arguments -> Effect Unit

instance invokeNoArgs :: InvokeHandler (Effect Unit) where
  invokeHandler _i = const

instance invokeWithArg ::
       (FromMax a, InvokeHandler b)
    => InvokeHandler (a -> b) where
  invokeHandler i f xs = invokeHandler (i + 1) (f (getArg xs i)) xs
