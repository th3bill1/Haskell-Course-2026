module Homework01 (main) where
import Data.Function

-- Ex. 1
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n
 | n > 3 = [ (x, n-x) | x <- [2..n`div`2], isPrime x, isPrime (n-x) ]
 | otherwise = error "n must be greater than 4"

-- Ex. 2
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs [] = []
coprimePairs (x:xs) = [ (x, y) | y <- xs, x < y, gcd x y == 1 ] ++ coprimePairs xs

-- Ex. 3
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime :: Int -> Bool
isPrime n = n `elem` primesTo n




main :: IO ()
main = do
  putStrLn "=== Homework 01 ==="

  putStrLn "\n-- Goldbach Pairs of 70--"
  print (goldbachPairs 70)

  putStrLn "\n-- Coprime Pairs of [4, 15, 20, 33, 69] --"
  print (coprimePairs [4, 15, 20, 33, 69])
