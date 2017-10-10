module Types where

import Prelude 
  ( show
  , (<>)
  )
import Data.Array
import Data.Maybe ( Maybe )

-- | Gene, a basic unit encoding a constant, a variable or a three-address code.
-- | Constants and variables are terminal symbols.
data Gene a i = C a | Var i | Op (F a) i i

showGene (C c) = show c
showGene (Var n) = "Var " <> show n
showGene (Op f i1 i2) = "Op " <> showF f <> " " <> show i1 <> " " <> show i2

-- | A Chromosome is an array of genes
newtype Chromosome a = Chromosome (Array (Gene a Int))

-- | A function and its symbolic representation
type F a = { s :: Char
           , f :: a -> a -> Maybe a
           }

showF :: forall a. F a -> String
showF { s: s } = show s
