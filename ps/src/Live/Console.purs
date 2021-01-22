module Live.Console (post) where

import Prelude
import Effect (Effect)

-- | Post to the Max console
-- |
-- | Instead of using `EffectFn`, we do the wrapping manually in `Console.js`.
-- | The reason is that Max's JavaScript environment is a little weird, and it
-- | it treats `post` special; this works just fine:
-- |
-- | ```javascript
-- | var fn = function(x) {
-- |   post("This is some other function", x, "\n");
-- | }
-- |
-- | var fnRenamed = fn;
-- | fnRenamed(2);
-- | ```
-- |
-- | but this does not:
-- |
-- | ```javascript
-- | var postRenamed = post;
-- | postRenamed("bye\n");
-- | ```
foreign import post :: String -> Effect Unit
