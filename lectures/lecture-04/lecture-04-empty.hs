{-# LANGUAGE InstanceSigs #-}

import Text.Read (readMaybe)
import Control.Exception
import Control.Concurrent (forkIO)

import Control.Monad
-- Can we fold other types?

data Option a = None | Some a
    deriving (Show, Eq)

foldrOption :: (a -> b) -> b -> Option a -> b
foldrOption f z None     = undefined
foldrOption f z (Some x) = undefined

instance Foldable Option where
    foldr :: (a -> b -> b) -> b -> Option a -> b
    foldr f z None     = undefined
    foldr f z (Some x) = undefined

data List a = Nil | Cons a (List a)
    deriving (Show, Eq)

instance Foldable List where
    foldr :: (a -> b -> b) -> b -> List a -> b
    foldr _ z Nil         = undefined
    foldr f z (Cons x xs) = undefined

sumFoldable :: (Foldable f, Num a) => f a -> a
sumFoldable x = undefined

lengthFoldable :: Foldable f => f a -> Int
lengthFoldable = undefined

-- exercise:
-- foldableToList :: Foldable f => f a -> [a]

-- | Back to IO!

-- game: Play a game.  `game n` is a game where the user tries to guess
-- `n`.  For every wrong step, tell the user if their guess is too high or
-- too low.
game :: Int -> IO ()
game n = undefined

-- data Ordering = LT | EQ | GT

-- data Maybe a = None | Just a

-- return the number of guesses taken
game' :: Int -> IO Int
game' n = runGameFrom 1
  where
    runGameFrom :: Int -> IO Int
    runGameFrom currScore = undefined

-- Actions as values!

doAll_ :: [IO a] -> IO ()
doAll_ []            = undefined
doAll_ (act1 : acts) = undefined

doAll :: [IO a] -> IO [a]
doAll []            = undefined
doAll (act1 : acts) = undefined

doAllParallel_ :: [IO ()] -> IO ()
doAllParallel_ []          = return ()
doAllParallel_ (act1:acts) = undefined

-- map :: (a -> b) -> [a] -> [b]

mapWithIO :: (a -> IO b) -> [a] -> IO [b]
mapWithIO f []     = undefined
mapWithIO f (x:xs) = undefined

-- | DSLs

-- works with a accumulator
data Action = Put String
            | Add Int
            | Set Int
            | DisplayAcc
            | ReadIn
  deriving (Show, Eq)

execAction :: Action -> Int -> IO Int
execAction (Put str) acc  = undefined
execAction (Add i)   acc  = undefined
execAction (Set i)   _    = undefined
execAction DisplayAcc acc = undefined
execAction ReadIn     acc = undefined

execProgram :: [Action] -> Int -> IO Int
execProgram [] acc = undefined
execProgram (act1 : acts) oldAcc = undefined

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

-- Monoid laws:
-- (x <> y) <> z = x <> (y <> z)
-- x <> mempty = x
-- mempty <> x = x


-- instance Monoid [a] where
--     mempty  = []
--     mappend x y = x ++ y

instance Monoid a => Monoid (Option a) where
    mappend (Some x) None     = Some x
    mappend None     (Some y) = Some y
    mappend (Some x) (Some y) = Some (mappend x y)

    mempty  = undefined

data Bool' = True' | False'
    deriving (Show, Eq)

-- XOR
instance Monoid Bool' where
    mappend True' False' = True'
    mappend False' True' = True'
    mappend _ _ = False'

    mempty = undefined

-- squishAllMonoids :: Monoid m => [m] -> m
-- squishAllMonoids [] = mempty
-- squishAllMonoids (x:xs) = x `mappend` squishAllMonoids xs

-- squishAllMonoids = foldl mappend mempty
squishAllMonoids = foldr mappend mempty

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
callbackOption None     f = undefined
callbackOption (Some x) f = undefined

instance Applicative Option where
    pure = return
    (<*>) = ap

instance Monad Option where
    return x = Some x
    None   >>= _ = undefined
    Some x >>= f = undefined

divideByTwo :: Int -> Option Int
divideByTwo x | even x    = undefined
              | otherwise = undefined

divideTwice :: Int -> Option Int
divideTwice x =
    divideByTwo x >>= undefined

divideTwice' :: Int -> Option Int
divideTwice' x = do
    y <- divideByTwo x
    undefined

data Either' e a = Left' e | Right' a
                 deriving (Show, Eq)

-- Option a = Either' () a

-- Either Int Bool -- Left' 4, Left' 7, Right' True, Right' False,

instance Functor (Either' e) where
    fmap :: (a -> b) -> Either' e a -> Either' e b
    fmap f (Left' x)  = undefined
    fmap f (Right' x) = undefined

cbEither :: Either e a -> (a -> Either e b) -> Either e b
cbEither (Left x) f  = undefined
cbEither (Right x) f = undefined

instance Applicative (Either' e) where
    pure = return
    (<*>) = ap

instance Monad (Either' e) where
    return x       = Right' x
    Left' x >>= f  = undefined
    Right' x >>= f = undefined

divideByTwoE :: Int -> Either' String Int
divideByTwoE i | even i    = Right' (i `div` 2)
               | otherwise = Left' "Not a multiple of 2"

divideByThreeE :: Int -> Either' String Int
divideByThreeE  i | i `mod` 3 == 0   = undefined
                  | otherwise = undefined

-- twoAndThree :: Int -> Either' String Int
-- twoAndThree x = do
--     y <- divideByTwoE x
--     z <- divideByThreeE y
--     divideByTwoE z

-- -- divide3Times x =
-- --     divideByTwo x >>= \y ->
-- --       divideByTwo y >>= \z ->
-- --         divideByTwo z
-- -- -- divideByTwo (divideByTwo 4)


