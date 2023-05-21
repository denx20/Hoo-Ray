-- arithmetic.hs
module Arithmetic (
    add,
    multiply,
    subtract,
    divide,
    sum,
    product
) where

import Prelude hiding (subtract, mapM, sum, product)
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

sum :: [Double] -> Double
sum x = foldl1' add x

product :: [Double] -> Double
product x = foldl1' multiply x
