module Pokus where

import Prelude hiding(sum)

square :: Int -> Int
square x = x*x

sum :: Int -> Int -> Int
sum x y = x + y

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

 
mcd :: Int -> Int -> Int
mcd x 0 = x
mcd x y = mcd y (mod x  y)

mcd2 :: Int -> Int -> Int
mcd2 x y | x == y  = x
         | x < y   = mcd2 x (y-x)
         | x > y   = mcd2 (x-y) y
