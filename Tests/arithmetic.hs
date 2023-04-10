-- arithmetic.hs
module Arithmetic (
    add,
    multiply,
    subtract,
    divide,
    exponentiate
) where

import Prelude hiding (subtract)
import Data.Fixed (mod')
import Data.List (foldl1')
import Data.Traversable (sequenceA)

add :: Double -> Double -> Double
add x y = x + y

multiply :: Double -> Double -> Double
multiply x y = x * y

subtract :: Double -> Double -> Double
subtract x y = x - y

divide :: Double -> Double -> Double
divide x y = x / y

exponentiate :: Double -> Double -> Double
exponentiate x y = x ** y
