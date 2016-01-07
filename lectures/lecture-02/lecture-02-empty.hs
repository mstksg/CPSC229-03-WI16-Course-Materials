{-# LANGUAGE InstanceSigs #-}

-- | Recursion fun

doubleAll :: Num a => [a] -> [a]
doubleAll []     = undefined
doubleAll (x:xs) = undefined

sumNums :: Num a => [a] -> a
sumNums []     = undefined
sumNums (x:xs) = undefined

numItems :: [a] -> Int
numItems []     = undefined
numItems (x:xs) = undefined

-- | Higher order functions

doubleAll' :: Num a => [a] -> [a]
doubleAll' xs = undefined

sumNums' :: Num a => [a] -> a
sumNums' xs = undefined

numItems' :: [a] -> Int
numItems' xs = undefined

-- | Not magic

map' :: (a -> b) -> [a] -> [b]
map' f []     = undefined
map' f (x:xs) = undefined

-- | Parametric polymorphism

id' :: a -> a
id' x = undefined

const' :: a -> b -> a
const' x y = undefined

const2' :: a -> b -> b
const2' x y = undefined

foo :: a -> a -> a
foo x y = undefined

-- not allowed
-- foo x y = if x is an Int
--               then 10
--               else y

-- | Function composition

numberOfEvens :: Integral a
               => [a] -> Int
numberOfEvens = length . filter even

-- doubleSquared x = (x * 2) ^ 2
doubleSquared = (^ 2) . (* 2)

-- | Recursion is still neat.  Works together with HOF's

qsort :: Ord a => [a] -> [a]
qsort []     = undefined
qsort (x:xs) = undefined

-- | Laziness

numsFrom :: Int -> [Int]
numsFrom n = undefined

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (n - 1) xs

myOr :: Bool -> Bool -> Bool
myOr True  _ = undefined
myOr False x = undefined

-- take' 2 (numsFrom 1) ?

-- Why is laziness important?

-- | Custom data types

-- type synonyms; textual substitution
type IntList = [Int]
-- type String = [Char]

-- new types
data Direction = N | E | S | W

instance Show Direction where
    show N = undefined
    show E = undefined
    show S = undefined
    show W = undefined

instance Eq Direction where
    N == N = undefined
    E == E = undefined
    S == S = undefined
    W == W = undefined
    _ == _ = undefined

turnRight :: Direction -> Direction
turnRight N = undefined
turnRight E = undefined
turnRight S = undefined
turnRight W = undefined

turnAround :: Direction -> Direction
turnAround N = undefined
turnAround E = undefined
turnAround S = undefined
turnAround W = undefined

data Bool' = True' | False'
    deriving (Show, Eq)

if' :: Bool' -> a -> a -> a
if' True'  thenCase elseCase = undefined
if' False' thenCase elseCase = undefined

-- | Data types containing other types
data Person = P String Integer
    deriving (Show, Eq)

teacher :: Person
teacher = P "Justin" 25

getName :: Person -> String
getName (P name age) = undefined

getAge :: Person -> Integer
getAge (P name age) = undefined

-- add 1 to the age
growUp :: Person -> Person
growUp (P name age) = undefined

sameAge :: Person -> Person -> Bool
sameAge (P name1 age1) (P name2 age2) = undefined

-- | Record syntax -- sugar only!
data Person' =
      P' { personName :: String
         , personAge  :: Integer
         }
    deriving (Show, Eq)

growUp' :: Person' -> Person'
growUp' p = undefined

sameAge' :: Person' -> Person' -> Bool
sameAge' p1 p2 = undefined

-- | Parameterized types

data OptionalInt = NoInt | SomeInt Int
    deriving (Show, Eq)

hasInt :: OptionalInt -> Bool
hasInt NoInt       = undefined
hasInt (SomeInt i) = undefined

addOptional :: Int
            -> OptionalInt
            -> OptionalInt
addOptional i NoInt       = undefined
addOptional i (SomeInt j) = undefined

safeDivide :: Int -> Int -> OptionalInt
safeDivide x 0 = undefined
safeDivide x y = undefined

firstInt :: [Int] -> OptionalInt
firstInt []     = undefined
firstInt (x:xs) = undefined

data OptionalString = NoString | SomeString String
    deriving (Show, Eq)

hasString :: OptionalString -> Bool
hasString NoString = undefined
hasString _        = undefined

firstString :: [String] -> OptionalString
firstString []     = undefined
firstString (x:xs) = undefined

concatOptional :: String -> OptionalString -> OptionalString
concatOptional str NoString          = undefined
concatOptional str (SomeString str') = undefined


data Option a = None | Some a
    deriving (Show, Eq)

hasSome :: Option a -> Bool
hasSome None     = undefined
hasSome (Some _) = undefined

firstVal :: [a] -> Option a
firstVal []     = undefined
firstVal (x:xs) = undefined

mapOption :: (a -> b) -> Option a -> Option b
mapOption f None     = undefined
mapOption f (Some x) = undefined


-- (a -> b) -> Option a -> Option b
-- (a -> b) -> [a] -> [b]



instance Functor Option where
    fmap :: (a -> b) -> Option a -> Option b
    fmap = mapOption




-- | Familiar faces, recursive types

-- Algebraic Data Type
-- ADT

data List a = Nil | Cons a (List a)
    deriving (Show, Eq)

sumList :: Num a => List a -> a
sumList Nil         = undefined
sumList (Cons x xs) = undefined

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap f Nil         = undefined
    fmap f (Cons x xs) = undefined

listToList :: List a -> [a]
listToList Nil         = undefined
listToList (Cons x xs) = undefined

