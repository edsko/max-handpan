module Util.SYB (
    collect
  ) where

import Data.Generics

-- | Collect all subterms of the appropriate type
--
-- TODO: Bit strange this doesn't exist as a standard combinator?
collect :: (Typeable a, Data b) => b -> [a]
collect = everything (++) (mkQ [] (:[]))
