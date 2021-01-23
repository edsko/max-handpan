module Live.Handlers (
     Handler -- opaque
   , mkHandler
   , setHandlers
   , class InvokeHandler
   , invokeHandler
   ) where

import Prelude

import Effect (Effect)

foreign import setHandlersImpl ::
     Array { message :: String, handler :: Effect Unit }
  -> Effect Unit

data Handler = MkHandler (forall r. (forall h. InvokeHandler h => h -> r) -> r)

mkHandler :: forall a. InvokeHandler a => a -> Handler
mkHandler h = MkHandler (\k -> k h)

setHandlers ::
     Array { message :: String, handler :: Handler }
  -> Effect Unit
setHandlers = setHandlersImpl <<< map invokeHandler'

invokeHandler' :: forall r.
     { handler :: Handler     | r }
  -> { handler :: Effect Unit | r }
invokeHandler' r@{ handler: MkHandler someHandler } =
    someHandler (\h -> r{handler = invokeHandler h})

class InvokeHandler a where
  invokeHandler :: a -> Effect Unit

instance invokeNoArgs :: InvokeHandler (Effect Unit) where
  invokeHandler = identity
