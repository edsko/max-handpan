-- | Methods available in the global JavaScript context
-- |
-- | https://docs.cycling74.com/max8/vignettes/jsglobal
module MaxForLive.Global (
    -- | Universally Available Methods
    post
  , postLn
    -- | Properties of `jsthis`
  , setInlets
  , setOutlets
  , numJsArgs
  , jsArg
    -- | `jsthis` Methods
  , outlet
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

import MaxForLive.Conversions (
    MaxValue
  , class FromMax
  , class ToMax
  , fromMax
  , toMax
  )

{-------------------------------------------------------------------------------
  Universally Available Methods
-------------------------------------------------------------------------------}

-- | Post to the Max console
foreign import post :: String -> Effect Unit

-- | Like 'post', but add a linebreak at the end
postLn :: String -> Effect Unit
postLn str = post (str <> "\n")

{-------------------------------------------------------------------------------
  Properties of `jsthis`
-------------------------------------------------------------------------------}

-- | Set number of patcher inlets
foreign import setInlets :: Int -> Effect Unit

-- | Set number of patcher outlets
foreign import setOutlets :: Int -> Effect Unit

-- | Get number of arguments to the `js` object
foreign import numJsArgs :: Int

-- | Get the specified argument
foreign import jsArgImpl :: Int -> MaxValue

-- | Get the @i@th argument
-- |
-- | This provides access to `jsarguments`. See
-- |
-- | * https://docs.cycling74.com/max8/vignettes/jsglobal#jsarguments
-- | * https://docs.cycling74.com/max8/tutorials/javascriptchapter03
-- |
-- | Throws an exception if the argument is of the wrong type.
jsArg :: forall a. FromMax a => Int -> a
jsArg = fromMax <<< jsArgImpl

{-------------------------------------------------------------------------------
  `jsthis` Methods
-------------------------------------------------------------------------------}

foreign import outletImpl :: EffectFn2 Int MaxValue Unit

-- | Send value on the specified outlet
outlet :: forall a. ToMax a => Int -> a -> Effect Unit
outlet i x = runEffectFn2 outletImpl i (toMax x)
