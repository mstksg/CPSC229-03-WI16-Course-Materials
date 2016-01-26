{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Attoparsec.Text hiding (D)
import Data.Text                   (Text)
import Test.QuickCheck
import qualified Data.Map          as M
import qualified Data.Text         as T
import qualified Data.Text.IO      as T

-- Parser Combinators

numberAndHello :: Parser (Int, Text)
numberAndHello = do
    d <- decimal
    str <- string "hello"
    return (d, str)

parseNChars :: Parser [Char]
parseNChars = do
    n <- decimal
    replicateM n anyChar


data User = U { userName  :: String
              , userID    :: Integer
              , userGroup :: Integer
              , userMeta  :: String
              , userHome  :: String
              , userShell :: String
              }
  deriving Show

parseUser :: Parser User
parseUser = do
    name <- passField
    _    <- passField
    uID  <- decimal
    _    <- char ':'
    gID  <- decimal
    _    <- char ':'
    metaD <- passField
    home  <- passField
    shell  <- manyTill anyChar (char '\n')
    return $ U name uID gID metaD home shell
  where
    passField :: Parser [Char]
    passField = manyTill anyChar (char ':')

parseUsers :: Parser [User]
parseUsers = many' parseUser

-- quickcheck

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lessThanX
            ++ [x]
            ++ qsort moreThanX
  where
    lessThanX = filter (< x) xs
                        -- bug?
    moreThanX = filter (>= x) xs

-- unit test:
-- qsort [1,6,2] === [1,2,6]
-- qsort [7,6,2] === [2,6,7]
--
-- property test:
-- length (qsort xs) == length xs
--
-- run:
-- > quickCheck prop_sameLength
-- > quickCheck prop_sorts
-- > quickCheck prop_noNew

prop_sameLength :: [Int] -> Bool
prop_sameLength xs = length xs == length (qsort xs)

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (x:xs) = all (>= x) xs
              && isSorted xs

prop_sorts :: [Int] -> Bool
prop_sorts xs = isSorted (qsort xs)

prop_noNew :: [Int] -> Bool
prop_noNew xs =
    let sorted = qsort xs
    in  all (`elem` xs) sorted

-- Concurrency

-- void foo(int *x) {
--   if *x != 0 {
--     return (*x / 0);     -- could have been changed by another thread!
--   } else {
--     return (3);
--   }
-- }

-- MVars: semaphores

testConcurrency :: MVar [String] -> IO ()
testConcurrency lst =
    forM_ ["hello","world","last"] (\s -> forkIO (do
        lst' <- takeMVar lst
        let newLst' = s : lst'
        putMVar lst newLst'
    ))


-- Cute math


-- automatic differentiation


data Diff a = D { dValue :: a
                , dDeriv :: a
                }
  deriving Show

instance Num a => Num (Diff a) where
    D x x' + D y y' = D (x + y) (x' + y')
    D x x' * D y y' = D (x * y) (x * y' + x' * y)
    D x x' - D y y' = D (x - y) (x' - y')
    -- abs (D x x')    = D (abs x) (if x >= 0 then x' else -x')
    fromInteger x   = D (fromInteger x) 0

instance Fractional a => Fractional (Diff a) where
    D x x' / D y y' = D (x / y)
                        ((y*x' - x*y')/(y*y))
    fromRational x = D (fromRational x) 0

instance Floating a => Floating (Diff a) where
    pi = D pi 0
    exp (D x x') = D (exp x) (x' * exp x)
    sin (D x x') = D (sin x) (x' * cos x)
    cos (D x x') = D (cos x) (-x' * sin x)
    sqrt (D x x') = D (sqrt x) (0.5 * x' / sqrt(x))
    log (D x x') = D (log x) (x' / x)

derivativeAt :: Num a => (Diff a -> Diff a) -> a -> a
derivativeAt f x = let D y y' = f (D x 1)
                   in  y'

-- power series

data PowerSeries = PS [Double]

-- 1 + x + 2 x^2
-- = [1,1,2]

evaluateAt :: PowerSeries -> Double -> Double
evaluateAt (PS ps) x = go ps 1
  where
    go [] y = 0
    go (c:cs) y = y * c + go cs (y*x)

taylorSeries :: (Diff a -> Diff a) -> PowerSeries
taylorSeries = undefined

instance Num PowerSeries where
    PS (x:xs) + PS (y:ys) = x+y : xs + ys
    PS []     + PS ys     = ys
    PS xs     + PS []     = xs


