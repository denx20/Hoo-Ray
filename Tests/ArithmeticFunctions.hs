{-# LANGUAGE OverloadedStrings #-}

module ArithmeticFunctions (
    add,
    mul,
    sub,
    divide,
    pow
) where

add :: Double -> Double -> Double
add x y = x + y

mul :: Double -> Double -> Double
mul x y = x * y

sub :: Double -> Double -> Double
sub x y = x - y

divide :: Double -> Double -> Double
divide x y = x / y

pow :: Double -> Double -> Double
pow x y = x ** y