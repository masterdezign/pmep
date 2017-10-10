module Random (
  withProbability
  , drawFrom
  ) where


import Prelude
import Data.Array
import Control.Monad.Eff.Random (
  random
  , randomInt
  , randomRange
  )
import Partial.Unsafe (unsafePartial)

drawFrom xs = do
  i <- randomInt 0 (len - 1)
  pure $ unsafePartial $ xs `unsafeIndex` i
    where len = length xs

withProbability p modify x = do
  t <- random
  if t < p
    then modify x
    else pure x
