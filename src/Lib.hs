module Lib (
  toDigits
  ) where


-- Exercise 1 
-- find the digits of a number

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0     = []
  | n < 10     = [n]
  | otherwise  = (n `mod` 10) : toDigitsRev(n `div` 10) 

toDigits :: Integer -> [Integer]
toDigits n = reverse( toDigitsRev n )
