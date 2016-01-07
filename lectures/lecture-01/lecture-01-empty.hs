-- | Lecture 1

-- | Values and Functions

double :: Int -> Int
double = undefined

stringTwice :: String -> String
stringTwice = undefined

-- | Multiple arguments
addTogether :: Int -> Int -> Int
addTogether = undefined

-- | Pattern matching
helloGoodbye :: Bool -> String
helloGoodbye True  = undefined
helloGoodbye False = undefined

-- helloGoodbye x =
--     case x of
--       True  -> undefined
--       False -> undefined

firstPlusSecond :: (Int, Int) -> Int
firstPlusSecond (x, y) = undefined

security :: String -> Bool
security "justin" = undefined
security _        = undefined

fib :: Int -> Int
fib 1 = undefined
fib 2 = undefined
fib n = undefined


-- | Guards
positive :: Int -> Bool
positive x | undefined = undefined
           | otherwise = undefined

factorial :: Int -> Int
factorial 1 = undefined
factorial n = undefined

-- | Operators
(*+*) :: Int -> Bool -> Int
x *+* True  = undefined
x *+* False = undefined

-- | Typeclasses
allEqual :: Eq a
         => a -> a -> a
         -> Bool
allEqual x y z = undefined

theMax :: Ord a => a -> a -> a
theMax x y | x > y     = undefined
           | otherwise = undefined

class Truthy a where
    isTruthy :: a -> Bool

instance Truthy Bool where
    isTruthy True  = undefined
    isTruthy False = undefined

instance Truthy Int where
    isTruthy 0 = undefined
    isTruthy _ = undefined

instance Truthy Double where
    isTruthy 0 = undefined
    isTruthy _ = undefined

truthyIf :: Truthy a
         => a
         -> b -> b
         -> b
truthyIf cond success fail
    | isTruthy cond = undefined
    | otherwise     = undefined

-- | Partial application
plus :: Num a => a -> (a -> a)
plus x = \y -> undefined

plus' :: Num a => (a, a) -> a
plus' (x, y) = undefined

plus3 :: Num a => a -> a
plus3 = plus undefined

-- | Lists

instance Truthy [a] where
    isTruthy [] = undefined
    isTruthy _  = undefined

getFst :: [String] -> String
getFst []     = undefined
getFst (x:xs) = undefined

head' :: [a] -> a
head' (x:xs) = undefined
head' []     = undefined

-- | Recursion

doubleAll :: Num a => [a] -> [a]
doubleAll []     = undefined
doubleAll (x:xs) = undefined

onlyEvens :: Integral a => [a] -> [a]
onlyEvens []     = undefined
onlyEvens (x:xs) = undefined

-- | Replcating iteration with
-- recursion

firstN :: Int -> [a] -> [a]
firstN 0 []     = undefined
firstN 0 (x:xs) = undefined
firstN n []     = undefined
firstN n (x:xs) = undefined

sumNums :: Num a => [a] -> a
sumNums []     = undefined
sumNums (x:xs) = undefined

