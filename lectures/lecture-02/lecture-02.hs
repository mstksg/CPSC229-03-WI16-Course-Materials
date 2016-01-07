{-# LANGUAGE InstanceSigs #-}

-- | Recursion fun

doubleAll :: Num a => [a] -> [a]
doubleAll []     = []
doubleAll (x:xs) = (x*2) : doubleAll xs

sumNums :: Num a => [a] -> a
sumNums [] = 0
sumNums (x:xs) = x + sumNums xs

numItems :: [a] -> Int
numItems [] = 0
numItems (x:xs) = 1 + numItems xs

-- | Higher order functions

doubleAll' :: Num a => [a] -> [a]
doubleAll' xs = map (* 2) xs

sumNums' :: Num a => [a] -> a
sumNums' xs = foldr (+) 0 xs

numItems' :: [a] -> Int
numItems' xs = foldr (\_ sumOfRest -> 1 + sumOfRest) 0 xs

-- | Not magic

map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

foldr' = undefined

foldl' = undefined

-- | Parametric polymorphism

id' :: a -> a
id' x = x

const' :: a -> b -> a
const' x y = x

const2' :: a -> b -> b
const2' x y = y

foo :: a -> a -> a
foo x y = x
-- foo x y = if x is an Int
--               then 10

-- | Function composition

numberOfEvens :: Integral a
               => [a] -> Int
numberOfEvens = length . filter even

-- doubleSquared x = (x * 2) ^ 2
doubleSquared = (^ 2) . (* 2)

-- | Recursion is still neat, works alongside HOF's

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lessThanX ++ [x] ++ qsort notLessThanX
  where
    lessThanX    = filter (< x) xs
    notLessThanX = filter (>= x) xs

-- | Laziness

numsFrom :: Int -> [Int]
numsFrom n = n : numsFrom (n + 1)

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (n - 1) xs

myOr :: Bool -> Bool -> Bool
myOr True  _ = True
myOr False x = x

-- take' 2 (numsFrom 1) ?

-- Why is laziness important?

-- | Custom data types

-- type synyoms; textual substitution
type IntList = [Int]
-- type String = [Char]

-- new types
data Direction = N | E | S | W

-- data Bool = False | True

instance Show Direction where
    show N = "N"
    show E = "E"
    show S = "S"
    show W = "W"

instance Eq Direction where
    N == N = True
    E == E = True
    S == S = True
    W == W = True
    N == E = False
    _ == _ = False

turnRight :: Direction -> Direction
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N

turnAround :: Direction -> Direction
turnAround N = S
turnAround E = W
turnAround S = N
turnAround W = E

data Bool' = True' | False'
    deriving (Show, Eq)

if' :: Bool' -> a -> a -> a
if' True' thenCase _  = thenCase
if' False' _ elseCase = elseCase

-- | Data types containing other types
data Person = P String Integer
    deriving (Show, Eq)

teacher :: Person
teacher = P "Justin" 25

getName :: Person -> String
getName (P name age) = name

getAge :: Person -> Integer
getAge (P name age) = age

-- add 1 to the age
growUp :: Person -> Person
growUp (P name age) = P name (age + 1)

sameAge :: Person -> Person -> Bool
sameAge (P name1 age1) (P name2 age2) = age1 == age2

-- | Record syntax -- sugar only!
data Person' = P' { personName :: String
                  , personAge  :: Integer
                  }
    deriving (Show, Eq)

growUp' :: Person' -> Person'
growUp' p = p { personAge = personAge p + 1 }

sameAge' :: Person' -> Person' -> Bool
sameAge' p1 p2 = personAge p1 == personAge p2

-- | Parameterized types

data OptionalInt = NoInt | SomeInt Int
    deriving (Show, Eq)

hasInt :: OptionalInt -> Bool
hasInt NoInt       = False
hasInt (SomeInt i) = True

addOptional :: Int -> OptionalInt -> OptionalInt
addOptional i NoInt       = NoInt
addOptional i (SomeInt j) = SomeInt (i + j)

safeDivide :: Int -> Int -> OptionalInt
safeDivide x 0 = NoInt
safeDivide x y = SomeInt (x `div` y)

firstInt :: [Int] -> OptionalInt
firstInt []     = NoInt
firstInt (x:xs) = SomeInt x

data OptionalString = NoString | SomeString String
    deriving (Show, Eq)

hasString :: OptionalString -> Bool
hasString NoString = False
hasString _        = True

firstString :: [String] -> OptionalString
firstString []     = NoString
firstString (x:xs) = SomeString x

concatOptional :: String -> OptionalString -> OptionalString
concatOptional str NoString          = NoString
concatOptional str (SomeString str') = SomeString (str ++ str')


data Option a = None | Some a
    deriving (Show, Eq)

hasSome :: Option a -> Bool
hasSome None     = False
hasSome (Some _) = True

firstVal :: [a] -> Option a
firstVal []     = None
firstVal (x:xs) = Some x

mapOption :: (a -> b) -> Option a -> Option b
mapOption f None     = None
mapOption f (Some x) = Some (f x)


-- mapOption :: (a -> b) -> Option a -> Option b
-- map       :: (a -> b) -> [a] -> [b]



instance Functor Option where
    fmap :: (a -> b) -> Option a -> Option b
    fmap = mapOption




-- | Familiar faces, recursive types

-- Algebraic Data Type
-- ADT

data List a = Nil | Cons a (List a)
    deriving (Show, Eq)

sumList :: Num a => List a -> a
sumList Nil         = 0
sumList (Cons x xs) = x + sumList xs

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap f Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

listToList :: List a -> [a]
listToList Nil         = []
listToList (Cons x xs) = x : listToList xs
