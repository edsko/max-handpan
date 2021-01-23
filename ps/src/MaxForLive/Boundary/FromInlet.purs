-- | Deal with input from Patcher inlets
module MaxForLive.Boundary.FromInlet (
    Arguments
  , class GetArg
  , getArg
  ) where

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

{-------------------------------------------------------------------------------
  Public API
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
