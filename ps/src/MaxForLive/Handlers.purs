-- | Set up Max message handlers
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsbasic
module MaxForLive.Handlers (
     -- | Max message handlers
     Handler -- opaque
   , mkHandler
   , setHandlers
     -- | FFI boundary
   , class GetArg
   , getArg
   , Arguments
   , class InvokeHandler
   , invokeHandler
   ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (
    EffectFn1
  , EffectFn2
  , mkEffectFn1
  , runEffectFn1
  , runEffectFn2
  )

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

invokeHandler' :: forall r.
     { handler :: Handler                  | r }
  -> { handler :: EffectFn1 Arguments Unit | r }
invokeHandler' r@{ handler: MkHandler someHandler } =
    someHandler (\h -> r{
        handler = mkEffectFn1 (\xs -> invokeHandler 0 xs h)
      })

{-------------------------------------------------------------------------------
  FFI: Functions of arbitrary arguments
-------------------------------------------------------------------------------}

class InvokeHandler a where
  invokeHandler :: Int -> Arguments -> a -> Effect Unit

instance invokeNoArgs :: InvokeHandler (Effect Unit) where
  invokeHandler _i _xs = identity

instance invokeWithArg ::
       (GetArg a, InvokeHandler b)
    => InvokeHandler (a -> b) where
  invokeHandler i xs f = getArg xs i >>= f >>> invokeHandler (i + 1) xs

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

-- | The Max 'Arguments' object
-- |
-- | This is entirely opaque to the PureScript code: we only pass it between
-- | the foreign functions.
foreign import data Arguments :: Type

class GetArg a where
  getArg :: Arguments -> Int -> Effect a

{-------------------------------------------------------------------------------
  Implementations for various types
-------------------------------------------------------------------------------}

foreign import getArgIntImpl :: EffectFn2 Arguments Int Int

instance getArgInt :: GetArg Int where
  getArg = runEffectFn2 getArgIntImpl
