module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array
import Data.Maybe

import Types
import Run (evaluate)

b'mult :: F Number
b'mult = {s: '*', f: \x y -> Just (x * y)}

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let pow8 = [Var 0, Op b'mult 0 0, Op b'mult 1 1, Op b'mult 2 2]
      result = evaluate (Chromosome pow8) [2.0]
  log $ show result
