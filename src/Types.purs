module Types where

import Data.Array
import Data.Maybe ( Maybe )

-- | Gene, a basic unit encoding a constant, a variable or a three-address code.
-- | Constants and variables are terminal symbols.
data Gene a i = C a | Var i | Op (F a) i i

-- | A Chromosome is an array of genes
newtype Chromosome a = Chromosome (Array (Gene a Int))

-- | A function and its symbolic representation
type F a = { s :: Char
           , f :: a -> a -> Maybe a
           }
