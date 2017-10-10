module Run ( evaluate ) where

import Prelude
import Control.Monad.ST ( runST )
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe
import Data.Array (foldl, foldr, snoc, unsafeIndex, (!!))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

import Types

-- If there is no corresponding variable in vmap, then place Nothing.
-- If operation f' results in Nothing, then place Nothing in acc.
-- Fail otherwise.
evaluate ::
  Chromosome Number
  -> Array Number
  -> Array (Maybe Number)
evaluate (Chromosome chr) vmap = foldl (\b g -> _f b g) [] chr
  where _f :: Array (Maybe Number) -> Gene Number Int -> Array (Maybe Number)
        _f acc (C c) = acc `snoc` (Just c)
        _f acc (Var n) = acc `snoc` (vmap !! n)
        _f acc (Op {f: f'} i1 i2) = acc `snoc` r
          where r = f' v1 v2
                -- Very unsafe
                v1 = unsafePartial $ fromJust (acc `unsafeIndex` i1)
                -- Twice as unsafe!
                v2 = unsafePartial $ fromJust (acc `unsafeIndex` i2)
