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

-- isPrime :: Int -> Bool
-- isPrime n = n `elem` primesTo n

-- Ex. 4
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b
 | null a || null b || null (head a) || null (head b) = error "Matrices cannot be empty"
 | length (head a) /= length b = error "Wrong dimensions"
 | otherwise = [ [ sum [ a !! i !! k * b !! k !! j | k <- [0 .. p-1] ] | j <- [0 .. n-1] ]| i <- [0 .. m-1] ]
 where
    m = length a
    p = length (head a)
    n = length (head b)

-- Ex. 5
permutations :: Int -> [a] -> [[a]]
permutations 0 _  = [[]]
permutations _ [] = []
permutations k xs =
  [ y : ys
  | (y, rest) <- select xs
  , ys <- permutations (k - 1) rest
  ]

select :: [a] -> [(a, [a])]
select []     = []
select (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- select xs ]

-- Ex. 6
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
 | x < y = x : merge xs (y:ys)
 | x > y = y : merge (x:xs) ys
 | otherwise = x : merge xs ys

hamming :: [Integer]
hamming = 1 : merge (map (2*) hamming) (merge (map (3*) hamming) (map (5*) hamming))

-- Ex. 7
power :: Int -> Int -> Int
power b e
 | e < 0 = error "Exponent must be non-negative"
 | otherwise = go 1 e
 where
    go !acc 0 = acc
    go !acc n = go (acc * b) (n - 1)

-- Ex. 8
listMaxSeq :: [Int] -> Int
listMaxSeq [] = error "Empty list"
listMaxSeq (x:xs) = go x xs
 where
    go acc [] = acc
    go acc (y:ys) =
      let acc' = max acc y
      in acc' `seq` go acc' ys

listMaxBang :: [Int] -> Int
listMaxBang [] = error "Empty list"
listMaxBang (x:xs) = go x xs
 where
    go !acc [] = acc
    go !acc (y:ys) = go (max acc y) ys

-- Ex. 9
primes :: [Int]
primes = sieve [2..]

isPrime :: Int -> Bool
isPrime n
 | n < 2 = False
 | otherwise = head (dropWhile (< n) primes) == n

-- Ex. 10
meanLazy :: [Double] -> Double
meanLazy [] = error "Empty list"
meanLazy xs = s / fromIntegral n
 where
    (s, n) = go xs (0, 0)
    go [] (sumSoFar, lenSoFar) = (sumSoFar, lenSoFar)
    go (y:ys) (sumSoFar, lenSoFar) = go ys (sumSoFar + y, lenSoFar + 1)

mean :: [Double] -> Double
mean [] = error "Empty list"
mean xs = s / fromIntegral n
 where
    (s, n) = go xs (0, 0)
    go [] (!sumSoFar, !lenSoFar) = (sumSoFar, lenSoFar)
    go (y:ys) (!sumSoFar, !lenSoFar) = go ys (sumSoFar + y, lenSoFar + 1)

meanVariance :: [Double] -> (Double, Double)
meanVariance [] = error "Empty list"
meanVariance xs = (m, v)
 where
    (s, s2, n) = go xs (0, 0, 0)
    m = s / fromIntegral n
    v = s2 / fromIntegral n - m * m

    go [] (!sumSoFar, !sumSqSoFar, !lenSoFar) = (sumSoFar, sumSqSoFar, lenSoFar)
    go (y:ys) (!sumSoFar, !sumSqSoFar, !lenSoFar) =
      go ys (sumSoFar + y, sumSqSoFar + y * y, lenSoFar + 1)

main :: IO ()
main = do
  putStrLn "=== Homework 01 ==="

  putStrLn "\n-- Goldbach Pairs of 70 --"
  print (goldbachPairs 70)

  putStrLn "\n-- Coprime Pairs of [4, 15, 20, 33, 69] --"
  print (coprimePairs [4, 15, 20, 33, 69])

  putStrLn "\n-- Matrix multiplication of [[1, 2], [3, 4]] and [[5, 6], [7, 8]] --"
  print (matMul [[1, 2], [3, 4]] [[5, 6], [7, 8]])

  putStrLn "\n-- Permutations of length 2 from [1, 2, 3] --"
  print (permutations 2 [1, 2, 3])

  putStrLn "\n-- First 20 Hamming numbers --"
  print (take 20 hamming)

  putStrLn "\n-- 2^10 --"
  print (power 2 10)

  putStrLn "\n-- listMaxSeq [4, 15, 20, 33, 69] --"
  print (listMaxSeq [4, 15, 20, 33, 69])

  putStrLn "\n-- listMaxBang [4, 15, 20, 33, 69] --"
  print (listMaxBang [4, 15, 20, 33, 69])

  putStrLn "\n-- First 21 primes --"
  print (take 21 primes)

  putStrLn "\n-- isPrime 2137 --"
  print (isPrime 2137)

  putStrLn "\n-- meanLazy [6, 7, 6, 9, 4, 2, 0] --"
  print (meanLazy [6, 7, 6, 9, 4, 2, 0])

  putStrLn "\n-- mean [6, 7, 6, 9, 4, 2, 0] --"
  print (mean [6, 7, 6, 9, 4, 2, 0])

  putStrLn "\n-- meanVariance [6, 7, 6, 9, 4, 2, 0] --"
  print (meanVariance [6, 7, 6, 9, 4, 2, 0])