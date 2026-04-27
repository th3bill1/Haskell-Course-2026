module Homework03 (main) where

import Control.Monad (guard)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.List (permutations)
import qualified Data.Map as Map
import Data.Map (Map)

-- Maybe Monad

type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

-- Ex. 1a
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
  neighbours <- Map.lookup pos maze
  Map.lookup dir neighbours

-- Ex. 1b
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath _ pos [] = Just pos
followPath maze pos (d:ds) = do
  next <- move maze pos d
  followPath maze next ds

-- Ex. 1c
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze start dirs = do
  rest <- go start dirs
  return (start : rest)
  where
    go _ [] = return []
    go pos (d:ds) = do
      next <- move maze pos d
      later <- go next ds
      return (next : later)

-- Ex. 2
type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (`Map.lookup` key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

-- List Monad

type Guest = String
type Conflict = (Guest, Guest)

-- Ex. 3
seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
  arrangement <- permutations guests
  guard (validSeating arrangement)
  return arrangement
  where
    validSeating [] = True
    validSeating [_] = True
    validSeating xs = all validPair neighbourPairs
      where
        neighbourPairs = zip xs (tail xs ++ [head xs])

    validPair (a, b) = not (conflictBetween a b)

    conflictBetween a b =
      (a, b) `elem` conflicts || (b, a) `elem` conflicts

-- Custom Monad

data Result a = Failure String | Success a [String]
  deriving (Eq, Show)

-- Ex. 4a
instance Functor Result where
  fmap _ (Failure msg) = Failure msg
  fmap f (Success x warnings) = Success (f x) warnings

instance Applicative Result where
  pure x = Success x []

  Failure msg <*> _ = Failure msg
  _ <*> Failure msg = Failure msg
  Success f warnings1 <*> Success x warnings2 =
    Success (f x) (warnings1 ++ warnings2)

instance Monad Result where
  Failure msg >>= _ = Failure msg
  Success x warnings1 >>= f =
    case f x of
      Failure msg -> Failure msg
      Success y warnings2 -> Success y (warnings1 ++ warnings2)

-- Ex. 4b
warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

-- Ex. 4c
validateAge :: Int -> Result Int
validateAge age
  | age < 0 = failure "Age cannot be negative"
  | age > 150 = do
      warn ("Unusually high age: " ++ show age)
      return age
  | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

-- Writer Monad

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
  deriving (Eq, Show)

-- Ex. 5
simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)
simplify (Neg e) = do
  e' <- simplify e
  case e' of
    Neg inner -> do
      tell ["Double negation: -(-e) -> e"]
      return inner
    _ -> return (Neg e')
simplify (Add e1 e2) = do
  e1' <- simplify e1
  e2' <- simplify e2
  case (e1', e2') of
    (Lit 0, e) -> do
      tell ["Add identity: 0 + e -> e"]
      return e
    (e, Lit 0) -> do
      tell ["Add identity: e + 0 -> e"]
      return e
    (Lit a, Lit b) -> do
      tell ["Constant folding: a + b -> c"]
      return (Lit (a + b))
    _ -> return (Add e1' e2')
simplify (Mul e1 e2) = do
  e1' <- simplify e1
  e2' <- simplify e2
  case (e1', e2') of
    (Lit 0, _) -> do
      tell ["Zero absorption: 0 * e -> 0"]
      return (Lit 0)
    (_, Lit 0) -> do
      tell ["Zero absorption: e * 0 -> 0"]
      return (Lit 0)
    (Lit 1, e) -> do
      tell ["Mul identity: 1 * e -> e"]
      return e
    (e, Lit 1) -> do
      tell ["Mul identity: e * 1 -> e"]
      return e
    (Lit a, Lit b) -> do
      tell ["Constant folding: a * b -> c"]
      return (Lit (a * b))
    _ -> return (Mul e1' e2')

-- Extra assignment

newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Eq, Show)

-- Ex. 6a
instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

main :: IO ()
main = do
  putStrLn "=== Homework 03 ==="

  let maze = Map.fromList
        [ ((0,0), Map.fromList [(E, (1,0)), (S, (0,1))])
        , ((1,0), Map.fromList [(W, (0,0)), (S, (1,1))])
        , ((0,1), Map.fromList [(N, (0,0)), (E, (1,1))])
        , ((1,1), Map.fromList [(N, (1,0)), (W, (0,1))])
        ]

  putStrLn "\n-- followPath maze (0,0) [E,S] --"
  print (followPath maze (0,0) [E,S])

  putStrLn "\n-- safePath maze (0,0) [E,S,W] --"
  print (safePath maze (0,0) [E,S,W])

  let key = Map.fromList [('a','h'), ('b','i'), ('c','!')]
  putStrLn "\n-- decrypt key \"abc\" --"
  print (decrypt key "abc")

  putStrLn "\n-- seatings [Alice,Bob,Carol] with Alice/Bob conflict --"
  print (seatings ["Alice", "Bob", "Carol"] [("Alice", "Bob")])

  putStrLn "\n-- validateAges [20,151,30] --"
  print (validateAges [20,151,30])

  putStrLn "\n-- simplify (Add (Lit 0) (Mul (Lit 1) (Lit 5))) --"
  print (runWriter (simplify (Add (Lit 0) (Mul (Lit 1) (Lit 5)))))

  putStrLn "\n-- ZipList tests --"
  print (pure id <*> ZipList [1,2,3])
  print (pure (+) <*> ZipList [1,2,3] <*> ZipList [10,20,30])
