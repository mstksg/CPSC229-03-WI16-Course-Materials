-- | Lecture 1

-- | Values and Functions

double :: Int -> Int
double x = x * 2

stringTwice :: String -> String
stringTwice str = str ++ str

-- stringTwice x
-- x ++ x

-- stringTwice (readFile "hello.txt")
-- readFile "hello.txt" ++ readFile "hello.txt"

-- | Multiple arguments
addTogether :: Int -> Int -> Int
-- addTogether x y = x + y
addTogether = (+)

-- | Pattern matching
-- helloGoodbye :: Bool -> String
helloGoodbye True  = "Hello!"
helloGoodbye _     = "Goodbye!"

-- helloGoodbye x =
--     case x of
--       True -> "hello"
--       False -> "goodbye"

firstPlusSecond :: (Int, Int) -> Int
firstPlusSecond (x, y) = x + y

-- security :: String -> Bool
security "justin" = True
security _        = False

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)


-- | Guards
positive :: Int -> Bool
positive x | x > 0     = True
           | otherwise = False
-- positive x = x > 0

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)

-- | Operators
(*+*) :: Int -> Bool -> Int
x *+* True  = x
x *+* False = -x

-- | Typeclasses
allEqual :: Eq a
         => a -> a -> a
         -> Bool
allEqual x y z = (x == y) && (y == z)

-- theMax :: Ord a => a -> a -> a
theMax x y | x > y     = x
           | otherwise = y

class Truthy a where
    isTruthy :: a -> Bool

instance Truthy Bool where
    isTruthy True  = True
    isTruthy False = False

instance Truthy Int where
    isTruthy 0 = False
    isTruthy _ = True

instance Truthy Double where
    isTruthy 0 = False
    isTruthy _ = True

truthyIf :: Truthy a
         => a
         -> b -> b
         -> b
truthyIf cond success fail
    | isTruthy cond = success
    | otherwise     = fail

-- | Partial application
plus :: Num a => a -> (a -> a)
-- plus x y = x + y -- sugar
plus x = (\y -> x + y) -- real

plus' :: Num a => (a, a) -> a
plus' (x, y) = x + y
-- fac 1

plus3 :: Num a => a -> a
plus3 = plus 3
-- plus3 = (\y -> 3 + y)

-- | Lists

instance Truthy [a] where
    isTruthy [] = False
    isTruthy _  = True

getFst :: [String] -> String
getFst []     = "empty list!"
getFst (x:xs) = x
    --  ^ first value
    --    ^ rest of the list

head' :: [a] -> a
head' (x:xs) = x
head' [] = undefined

-- | Recursion

doubleAll :: Num a => [a] -> [a]
doubleAll [] = []
doubleAll (x:xs) =
    (x * 2) : doubleAll xs

onlyEvens :: Integral a
          => [a] -> [a]
onlyEvens []     = []
onlyEvens (x:xs)
  | even x    = x : onlyEvens xs
  | otherwise = onlyEvens xs

-- | Replcating iteration with
-- recursion

firstN :: Int -> [a] -> [a]
firstN 0 []     = []
firstN 0 (x:xs) = []
firstN n [] = []
firstN n (x:xs) =
    x : firstN (n - 1) xs

sumNums :: Num a => [a] -> a
sumNums [] = 0
sumNums (x:xs) = x + sumNums xs
