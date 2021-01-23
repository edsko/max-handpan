-- | Set up Max message handlers
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsbasic
module MaxForLive.Handlers (
     -- | Max message handlers
     registerHandler
     -- | FFI boundary
   , class InvokeHandler
   , invokeHandler
   ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (
    EffectFn1
  , EffectFn2
  , mkEffectFn1
  , runEffectFn2
  )

import MaxForLive.Arguments (Arguments, getArg)
import MaxForLive.Conversions (class FromMax)

foreign import registerHandlerImpl ::
     EffectFn2 String (EffectFn1 Arguments Unit) Unit

{-------------------------------------------------------------------------------
  Max message handlers
-------------------------------------------------------------------------------}

-- | Register Max message handlers
-- |
-- | For example, to respond to a bang:
-- |
-- | ```purescript
-- | registerHandler "bang" $ postLn "BANG!"
-- | ```
-- |
-- | Use `"msg_int"` and co as `message` to handle values as messages, see
-- | https://docs.cycling74.com/max8/vignettes/jsbasic#Special_Function_Names
registerHandler :: forall a. InvokeHandler a => String -> a -> Effect Unit
registerHandler msg h =
    runEffectFn2 registerHandlerImpl msg $
      mkEffectFn1 (invokeHandler 0 h)

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
