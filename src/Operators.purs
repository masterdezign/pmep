module Operators where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (
  random
  , randomInt
  , randomRange
  , RANDOM
  )

import Types
import Random

type Config a =
  {
    p'const :: Number        -- ^ Probability of constant generation
    , p'var :: Number        -- ^ Probability of variable generation.
                             -- The probability of operator generation is inferred
                             -- automatically as @1 - p'const - p'var@.
    , p'mutation :: Number   -- ^ Mutation probability
    , p'crossover :: Number  -- ^ Crossover probability

    , c'length :: Int        -- ^ The chromosome length
    , c'popSize :: Int       -- ^ A (sub)population size
    , c'popN :: Int          -- ^ Number of subpopulations (1 or more)  [not implemented]
    , c'ops :: Array (F a)   -- ^ Functions pool with their symbolic
                             -- representations
    , c'vars :: Int          -- ^ The input dimensionality
  }

newVar :: forall a e.
  Int
  -> Eff ( random :: RANDOM | e ) (Gene a Int)
newVar vars = Var <$> randomInt 0 (vars - 1)

newOp :: forall a e.
  Array (F a)
  -> Int
     -> Eff
          ( random :: RANDOM
          | e
          )
          (Gene a Int)
newOp ops maxIndex = do
  op <- drawFrom ops
  i1 <- randomInt 0 (maxIndex - 1)
  i2 <- randomInt 0 (maxIndex - 1)
  pure $ Op op i1 i2

-- | Draw a constant from the uniform distribution within @[-0.5, 0.5)@
newC :: forall a.
  Eff
    ( random :: RANDOM
    | a
    )
    (Gene Number Int)
newC = C <$> randomRange (-0.5) 0.5
