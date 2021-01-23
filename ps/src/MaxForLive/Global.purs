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
    -- | `jsthis` Methods
  , outlet
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

import MaxForLive.Conversions (MaxValue, class ToMax, toMax)

{-------------------------------------------------------------------------------
  Universally Available Methods
-------------------------------------------------------------------------------}

-- | Post to the Max console
--
-- Instead of using `EffectFn`, we do the wrapping manually in `Console.js`.
-- The reason is that Max's JavaScript environment is a little weird, and it
-- it treats `post` special; this works just fine:
--
-- ```javascript
-- var fn = function(x) {
--   post("This is some other function", x, "\n");
-- }
--
-- var fnRenamed = fn;
-- fnRenamed(2);
-- ```
--
-- but this does not:
--
-- ```javascript
-- var postRenamed = post;
-- postRenamed("bye\n");
-- ```
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

{-------------------------------------------------------------------------------
  `jsthis` Methods
-------------------------------------------------------------------------------}

foreign import outletImpl :: EffectFn2 Int MaxValue Unit

-- | Send value on the specified outlet
outlet :: forall a. ToMax a => Int -> a -> Effect Unit
outlet i x = runEffectFn2 outletImpl i (toMax x)
