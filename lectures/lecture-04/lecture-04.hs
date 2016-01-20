{-# LANGUAGE InstanceSigs #-}

import Text.Read (readMaybe)
import Control.Exception
import Control.Concurrent (forkIO)

import Control.Monad
-- Laziness can also help performance!

foo :: [Int]
foo = (take 10 . filter even . map (+9)) [1..100000000]
-- take 10 (filter even (map (+9) [1..100000]))

-- see lazy.js: http://danieltao.com/lazy.js/

-- Can we fold other types?

data Option a = None | Some a
    deriving (Show, Eq)

foldrOption :: (a -> b) -> b -> Option a -> b
foldrOption f z None     = z
foldrOption f z (Some x) = f x

instance Foldable Option where
    foldr :: (a -> b -> b) -> b -> Option a -> b
    foldr f z None     = z
    foldr f z (Some x) = f x z

data List a = Nil | Cons a (List a)
    deriving (Show, Eq)

instance Foldable List where
    foldr :: (a -> b -> b) -> b -> List a -> b
    foldr _ z Nil         = z
    foldr f z (Cons x xs) = f x (foldr f z xs)

sumFoldable :: (Foldable f, Num a) => f a -> a
sumFoldable x = foldr (+) 0 x

lengthFoldable :: (Foldable f) => f a -> Int
lengthFoldable = foldl (\oldAcc _ -> oldAcc + 1) 0

-- exercise:
-- foldableToList :: Foldable f => f a -> [a]

-- | Back to IO!

-- game: Play a game.  `game n` is a game where the user tries to guess
-- `n`.  For every wrong step, tell the user if their guess is too high or
-- too low.
game :: Int -> IO ()
game n = do
    putStrLn "What is the number?"
    answer <- getLine
    case readMaybe answer of
      Nothing -> do
        putStrLn "Could not parse!  Try again!"
        game n
      Just g ->
        case compare g n of
          LT -> do
            putStrLn "It's too low!"
            game n
          GT -> do
            putStrLn "Too high!"
            game n
          EQ ->
            putStrLn "Good job!  You win!!!"

-- data Ordering = LT | EQ | GT

-- data Maybe a = None | Just a

-- return the number of guesses taken
game' :: Int -> IO Int
game' n = runGameFrom 1
  where
    runGameFrom :: Int -> IO Int
    runGameFrom currScore = do
      putStrLn "What is the number?"
      answer <- getLine
      case readMaybe answer of
        Nothing -> do
          putStrLn "Could not parse!  Try again!"
          runGameFrom currScore
        Just g ->
          case compare g n of
            LT -> do
              putStrLn "It's too low!"
              runGameFrom (currScore + 1)
            GT -> do
              putStrLn "Too high!"
              runGameFrom (currScore + 1)
            EQ -> do
              putStrLn "Good job!  You win!!!"
              putStrLn ("You had " ++ show currScore ++ " guesses.")
              return currScore

-- Actions as values!

doAll_ :: [IO a] -> IO ()
doAll_ [] = return ()
doAll_ (act1 : acts) = do
    act1
    doAll_ acts

doAll :: [IO a] -> IO [a]
doAll [] = return []
doAll (act1 : acts) = do
    res <- act1
    reses <- doAll acts
    return (res : reses)

doAllParallel_ :: [IO ()] -> IO ()
doAllParallel_ [] = return ()
doAllParallel_ (act1:acts) =  do
    forkIO act1         -- do act1 on a new thread
    doAllParallel_ acts

-- map :: (a -> b) -> [a] -> [b]

mapWithIO :: (a -> IO b) -> [a] -> IO [b]
mapWithIO f [] = return []
mapWithIO f (x:xs) = do
    y <- f x
    ys <- mapWithIO f xs
    return (y:ys)


-- | DSLs

-- works with a accumulator
data Action = Put String
            | Add Int
            | Set Int
            | DisplayAcc
            | ReadIn
  deriving (Show, Eq)

execAction :: Action -> Int -> IO Int
execAction (Put str) acc = do
    putStrLn str
    return acc
execAction (Add i) acc =
    return (acc + i)
execAction (Set i) _   =
    return i
execAction DisplayAcc acc = do
    print acc
    return acc
execAction ReadIn acc = do
    res <- getLine
    case readMaybe res of
      Nothing -> do
        putStrLn "no parse."
        return acc
      Just newAcc ->
        return newAcc

execProgram :: [Action] -> Int -> IO Int
execProgram [] acc = return acc
execProgram (act1 : acts) oldAcc = do
    newAcc <- execAction act1 oldAcc
    execProgram acts newAcc

-- [Action]
sequencePrograms :: [Action] -> [Action] -> [Action]
sequencePrograms x y = x ++ y

-- | Useful typeclasses

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

instance Functor Option where
    fmap :: (a -> b) -> Option a -> Option b
    fmap f None = None
    fmap f (Some x) = Some (f x)

-- class Monoid m where
--     mempty  :: m
--     mappend :: m -> m -> m

-- instance Monoid [a] where
--     mempty  = []
--     mappend x y = x ++ y

instance Monoid a => Monoid (Option a) where
    mempty  = None
    mappend (Some x) None     = Some x
    mappend None     (Some y) = Some y
    mappend (Some x) (Some y) = Some (mappend x y)

data Bool' = True' | False'
    deriving (Show, Eq)

-- XOR
instance Monoid Bool' where
    mempty :: Bool'
    mempty = False'
    mappend :: Bool' -> Bool' -> Bool'
    mappend True' False' = True'
    mappend False' True' = True'
    mappend _ _ = False'

-- Monoid laws:
-- (x <> y) <> z = x <> (y <> z)
-- x <> mempty = x
-- mempty <> x = x

-- squishAllMonoids :: Monoid m => [m] -> m
-- squishAllMonoids [] = mempty
-- squishAllMonoids (x:xs) = x `mappend` squishAllMonoids xs

-- squishAllMonoids = foldl mappend mempty
-- squishAllMonoids = foldr mappend mempty

-- [x,y,z,a]

-- (x<>y) <> (z<>a)



-- class Monad m where
--     return :: a -> m a
--     (>>=)   :: m a -> (a -> m b) -> m b
--                ^ original action
--                        ^ callback to attach
--                                      ^ attached to callback

-- instance Monad IO where
--     return = return
--     act1 >>= callback = do
--       res <- act1
--       callback res

callbackOption :: Option a -> (a -> Option b) -> Option b
callbackOption None     _ = None
callbackOption (Some x) f = f x

instance Applicative Option where
    pure = return
    (<*>) = ap

instance Monad Option where
    return x = Some x
    None   >>= _ = None
    Some x >>= f = f x

divideByTwo :: Int -> Option Int
divideByTwo x | even x    = Some (x `div` 2)
              | otherwise = None

greet :: IO ()
greet = putStrLn "hello, what is your name?" >>= (\_ ->
          getLine >>= (\res ->
            putStrLn ("hello, " ++ res) )
          )

greet2 :: IO ()
greet2 =
    putStrLn "hello"             >>= \_ ->
    getLine                      >>= \str ->
    putStrLn ("Hello, " ++ str) 

greet3 :: IO ()
greet3 = do
    putStrLn "hello"
    str <- getLine
    putStrLn ("hello" ++ str)

divideTwice :: Int -> Option Int
divideTwice x =
    divideByTwo x >>= (\y -> divideByTwo y)

-- divideTwice 12
-- divideByTwo 12 >>= (\y -> divideByTwo y)
-- Some 6         >>= (\y -> divideByTwo y)
--                           divideByTwo 6
--                           Some 3
--


divideTwice' :: Int -> Option Int
divideTwice' x = do
    y <- divideByTwo x
    divideByTwo y

    -- divideByTwo x >>= \y ->
    -- divideByTwo y

divide3Times :: Int -> Option Int
divide3Times x = do         -- x = 80           -- x = 14
    y <- divideByTwo x      -- y = 40           -- y = 7
    z <- divideByTwo y      -- z = 20           -- None
    divideByTwo z           -- answer is 10     -- None

data Either' e a = Left' e | Right' a
                 deriving (Show, Eq)

-- Option a = Either' () a

-- Either Int Bool -- Left' 4, Left' 7, Right' True, Right' False,

instance Functor (Either' e) where
    fmap :: (a -> b) -> Either' e a -> Either' e b
    fmap f (Left' x)  = Left' x
    fmap f (Right' x) = Right' (f x)

cbEither :: Either e a -> (a -> Either e b) -> Either e b
cbEither (Left x) f  = Left x
cbEither (Right x) f = f x

instance Applicative (Either' e) where
    pure = return
    (<*>) = ap

instance Monad (Either' e) where
    return x = Right' x
    Left' x >>= f = Left' x     -- we errored out
    Right' x >>= f = f x

divideByTwoE :: Int -> Either' String Int
divideByTwoE i | even i = Right' (i `div` 2)
               | otherwise = Left' "Not a multiple of 2"

divideByThreeE :: Int -> Either' String Int
divideByThreeE  i | i `mod` 3 == 0   = Right' (i `div` 3)
                  | otherwise = Left' "Not a multiple of 3"

twoAndThree :: Int -> Either' String Int
twoAndThree x = do
    y <- divideByTwoE x
    z <- divideByThreeE y
    divideByTwoE z

-- divide3Times x =
--     divideByTwo x >>= \y ->
--       divideByTwo y >>= \z ->
--         divideByTwo z
-- -- divideByTwo (divideByTwo 4)

