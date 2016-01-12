{-# LANGUAGE InstanceSigs #-}

import Text.Read (readMaybe)
import Control.Exception

-- Equational reasoning

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- (f . g) = \x -> f (g x)

-- f . id = f

data Option a = None | Some a
    deriving (Show, Eq)

mapOption :: (a -> b) -> Option a -> Option b
mapOption _ None     = undefined
mapOption f (Some x) = undefined

-- mapOption id = id

-- mapOption f . mapOption g = mapOption (f . g)

data List a = Nil | Cons a (List a)
    deriving (Show, Eq)

mapList :: (a -> b) -> List a -> List b
mapList _ Nil         = undefined
mapList f (Cons x xs) = undefined

-- mapList id = id

-- mapList f . mapList g = mapList (f . g)

-- | Folds

-- Imperative "chomping":
foldl' :: (b -> a -> b) -> b -> [a] -> [b]
foldl' = undefined

-- Functional "deconstruction":
foldr' :: (a -> b -> b) -> b -> [a] -> [b]
foldr' = undefined

anyTrue :: [Bool] -> Bool
anyTrue = undefined

-- Deconstructing other things
foldOption :: (a -> b) -> b -> Option a -> b
foldOption = undefined

instance Foldable List where
    foldr :: (a -> b -> b) -> b -> List a -> b
    foldr = undefined

instance Foldable Option where
    foldr :: (a -> b -> b) -> b -> Option a -> b
    foldr = undefined

-- exercise:
-- toList :: Foldable f => f a -> [a]

-- | Trees

data Tree a = Leaf a | Branch (Tree a) (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap = undefined

-- | Binary search trees

insert :: Ord a => a -> Tree a -> Tree a
insert = undefined

findInTree :: Ord a => (a -> Bool) -> Tree a -> Option a
findInTree = undefined

-- exercise:
-- remove :: Ord a => (a -> Bool) -> Tree a -> Tree a

-- | IO

sayHello :: IO ()
sayHello = putStrLn "Hello!"

sayHelloTwice :: IO ()
sayHelloTwice = do
    putStrLn "Hello!"
    putStrLn "Hello again!"

doTwice :: IO a -> IO a
doTwice action = do
    action
    action

greet :: IO ()
greet = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")

doTwice' :: IO a -> IO (a, a)
doTwice' action = do
    res1 <- action
    res2 <- action
    return (res1, res2)

-- data Maybe a = Nothing | Just a

readLineOption :: Read a => IO (Maybe a)
readLineOption = undefined

-- instance Functor IO where
--     fmap :: (a -> b) -> IO a -> IO b

game :: Int -> IO ()
game = undefined

-- keep track of number of guesses
game' :: Int -> IO Int
game' = undefined

doForever :: IO a -> IO ()
doForever = undefined

while :: IO Bool -> IO ()
while = undefined

doAll_ :: [IO a] -> IO ()
doAll_ = undefined

doAll :: [IO a] -> IO [a]
doAll = undefined

doAllParallel_ :: [IO a] -> IO ()
doAllParallel_ = undefined

doWhen :: Bool -> IO () -> IO ()
doWhen = undefined

-- | Exceptions

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe = undefined

-- | Building DSL's

data Action = Put String
            | Add Int
            | Set Int
            | Display
            | ReadIn
  deriving (Show, Eq)

execAction :: Action -> Int -> IO Int
execAction = undefined

execProgram :: [Action] -> Int -> IO Int
execProgram = undefined

-- | Typeclasses and Laws

-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool

-- class Ord a where
--     (<) :: a -> a -> Bool
--     (>) :: a -> a -> Bool
--     (>=) :: a -> a -> Bool
--     (<=) :: a -> a -> Bool

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- class Monoid m where
--     mempty  :: m
--     mappend :: m -> m -> m

instance Monoid (List a) where
    mempty = undefined
    mappend = undefined

instance Monoid a => Monoid (Option a) where
    mempty = undefined
    mappend = undefined

instance Monoid Int where
    mempty = undefined
    mappend = undefined

newtype Summer a = Summer a
    deriving (Show, Eq)

newtype Multer a = Multer a
    deriving (Show, Eq)

instance Num a => Monoid (Summer a) where
    mempty = undefined
    mappend = undefined

instance Num a => Monoid (Multer a) where
    mempty = undefined
    mappend = undefined

instance Monoid a => Monoid (r -> a) where
    mempty = undefined
    mappend = undefined

-- | Tour of types and typeclasses

-- | Either

data Either' a b = Left' a | Right' b
    deriving (Show, Eq)

instance Functor (Either' e) where
    fmap :: (a -> b) -> Either' e a -> Either' e b
    fmap = undefined

-- | Writer

data Writer w a = Writer w a
    deriving (Show, Eq)

instance Functor (Writer w) where
    fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap = undefined

-- | Reader

data Reader r a = Reader { runReader :: r -> a }
    deriving (Show, Eq)

-- | Applicative

overOptions :: (a -> b -> c) -> Option a -> Option b -> Option c
overOptions = undefined

overEithers :: (a -> b -> c) -> Either' e a -> Either' e b -> Either' e c
overEithers = undefined

overReaders :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
overReaders = undefined

overWriters :: Monoid w => (a -> b -> c) -> Writer w a -> Writer w b -> Writer w c
overWriters = undefined

overIOs :: (a -> b -> c) -> IO a -> IO b -> IO c
overIOs = undefined

instance Applicative Option where
    pure :: a -> Option a
    pure = undefined
    (<*>) :: Option (a -> b) -> Option a -> Option b
    (<*>) = undefined

instance Applicative (Either' e) where
    pure :: a -> Either' e a
    pure = undefined
    (<*>) :: Either' e (a -> b) -> Either' e a -> Either' e b
    (<*>) = undefined


