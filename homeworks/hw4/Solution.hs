module Homework04 (main) where

newtype Reader r a = Reader { runReader :: r -> a }

-- Ex. 1
instance Functor (Reader r) where
  fmap f reader = Reader $ \env -> f (runReader reader env)

instance Applicative (Reader r) where
  pure x = Reader $ \_ -> x

  liftA2 f readerA readerB = Reader $ \env ->
    f (runReader readerA env) (runReader readerB env)

  readerF <*> readerA = Reader $ \env ->
    runReader readerF env (runReader readerA env)

instance Monad (Reader r) where
  reader >>= f = Reader $ \env ->
    runReader (f (runReader reader env)) env

-- Ex. 2
ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

local :: (r -> r) -> Reader r a -> Reader r a
local f reader = Reader $ \env -> runReader reader (f env)

-- Ex. 3
data BankConfig = BankConfig
  { interestRate   :: Double
  , transactionFee :: Int
  , minimumBalance :: Int
  } deriving (Eq, Show)

data Account = Account
  { accountId :: String
  , balance   :: Int
  } deriving (Eq, Show)

calculateInterest :: Account -> Reader BankConfig Int
calculateInterest account = do
  rate <- asks interestRate
  return (floor (fromIntegral (balance account) * rate))

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee account = do
  fee <- asks transactionFee
  return account { balance = balance account - fee }

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance account = do
  minBalance <- asks minimumBalance
  return (balance account >= minBalance)

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount account = do
  accountAfterFee <- applyTransactionFee account
  interest <- calculateInterest account
  hasMinimum <- checkMinimumBalance account
  return (accountAfterFee, interest, hasMinimum)

main :: IO ()
main = do
  putStrLn "=== Homework 04 ==="

  let cfg = BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 }
  let acc = Account { accountId = "A-001", balance = 1000 }

  putStrLn "\n-- processAccount acc --"
  print (runReader (processAccount acc) cfg)

  putStrLn "\n-- calculateInterest acc with locally doubled interest rate --"
  print (runReader (local (\c -> c { interestRate = 0.10 }) (calculateInterest acc)) cfg)
