module Homework02 (main) where

import Data.Foldable (toList)

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
  deriving (Eq, Show)

-- Ex. 1
instance Functor Sequence where
  fmap _ Empty        = Empty
  fmap f (Single x)   = Single (f x)
  fmap f (Append l r) = Append (fmap f l) (fmap f r)

-- Ex. 2
instance Foldable Sequence where
  foldMap _ Empty        = mempty
  foldMap f (Single x)   = f x
  foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList = toList

seqLength :: Sequence a -> Int
seqLength = length

-- Ex. 3
instance Semigroup (Sequence a) where
  Empty <> s = s
  s <> Empty = s
  s1 <> s2   = Append s1 s2

instance Monoid (Sequence a) where
  mempty = Empty

-- Ex. 4
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem x seq0 = go [seq0]
  where
    go [] = False
    go (Empty:rest) = go rest
    go (Single y:rest) = x == y || go rest
    go (Append l r:rest) = go (l:r:rest)

-- Ex. 5
tailToList :: Sequence a -> [a]
tailToList seq0 = reverse (go [seq0] [])
  where
    go [] acc = acc
    go (Empty:rest) acc = go rest acc
    go (Single x:rest) acc = go rest (x:acc)
    go (Append l r:rest) acc = go (l:r:rest) acc

-- Ex. 5 / RPN
data Token = TNum Int | TAdd | TSub | TMul | TDiv
  deriving (Eq, Show)

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [result] = Just result
    go [] _        = Nothing

    go (TNum n:ts) stack = go ts (n:stack)

    go (TAdd:ts) (x:y:stack) = go ts ((y + x):stack)
    go (TSub:ts) (x:y:stack) = go ts ((y - x):stack)
    go (TMul:ts) (x:y:stack) = go ts ((y * x):stack)
    go (TDiv:ts) (0:_:_)     = Nothing
    go (TDiv:ts) (x:y:stack) = go ts ((y `div` x):stack)

    go (_:_) _ = Nothing

-- Ex. 6
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr step []
  where
    step x acc
      | p x       = x : acc
      | otherwise = []

decimal :: [Int] -> Int
decimal = foldl (\acc digit -> acc * 10 + digit) 0

-- Ex. 7
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x [] = [(x, 1)]
    step x ((y, n):rest)
      | x == y    = (y, n + 1) : rest
      | otherwise = (x, 1) : (y, n) : rest

decode :: [(a, Int)] -> [a]
decode = foldr step []
  where
    step (x, n) acc = replicate n x ++ acc

main :: IO ()
main = do
  putStrLn "=== Homework 02 ==="

  let seq1 = Append (Single 1) (Append (Single 2) (Single 3))
  let seq2 = Append Empty (Single 4)

  putStrLn "\n-- fmap (*2) seq1 --"
  print (fmap (*2) seq1)

  putStrLn "\n-- seqToList seq1 --"
  print (seqToList seq1)

  putStrLn "\n-- seqLength seq1 --"
  print (seqLength seq1)

  putStrLn "\n-- seqToList (seq1 <> seq2) --"
  print (seqToList (seq1 <> seq2))

  putStrLn "\n-- tailElem 2 seq1 --"
  print (tailElem 2 seq1)

  putStrLn "\n-- tailToList seq1 --"
  print (tailToList seq1)

  putStrLn "\n-- tailRPN [3, 4, +, 2, *] --"
  print (tailRPN [TNum 3, TNum 4, TAdd, TNum 2, TMul])

  putStrLn "\n-- myReverse [1,2,3,4] --"
  print (myReverse [1,2,3,4])

  putStrLn "\n-- myTakeWhile even [2,4,3,6] --"
  print (myTakeWhile even [2,4,3,6])

  putStrLn "\n-- decimal [1,2,3] --"
  print (decimal [1,2,3])

  putStrLn "\n-- encode \"aaabccca\" --"
  print (encode "aaabccca")

  putStrLn "\n-- decode [('a',3),('b',1),('c',3),('a',1)] --"
  print (decode [('a',3),('b',1),('c',3),('a',1)])
