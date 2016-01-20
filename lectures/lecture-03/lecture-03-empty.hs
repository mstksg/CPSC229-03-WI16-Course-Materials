{-# LANGUAGE InstanceSigs #-}

-- Equational reasoning

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- (f . g) x = f (g x)
-- id x = x

-- PROVE: f . id = f

data Option a = None | Some a
    deriving (Show, Eq)

mapOption :: (a -> b) -> Option a -> Option b
mapOption f None     = None
mapOption f (Some x) = Some (f x)

-- PROVE: mapOption id = id

-- PROVE: mapOption f . mapOption g = mapOption (f . g)
--
-- aka: mapOption f (mapOption g x) = mapOption (f . g) x

data List a = Nil | Cons a (List a)
    deriving (Show, Eq)

mapList :: (a -> b) -> List a -> List b
mapList _ Nil         = Nil
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

-- excercises!
-- PROVE: mapList id = id
-- PROVE: mapList f . mapList g = mapList (f . g)

-- | Folds

-- sum :: [Int] -> Int
-- length :: [a] -> Int
-- product :: [Int] -> Int

-- use a fold: foldr, or foldl?
anyTrue :: [Bool] -> Bool
anyTrue = undefined

-- | Trees

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Leaf = undefined
    fmap f (Node x tL tR) = undefined

-- | Binary search trees

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree = undefined

-- use foldl
constructTree :: Ord a => [a] -> Tree a
constructTree = undefined

isInTree :: Ord a
         => a -> Tree a -> Bool
isInTree = undefined

sumTree :: Num a
        => Tree a -> a
sumTree = undefined

-- exercise:
-- remove :: Ord a => a -> Tree a -> Tree a

-- | IO

-- data () = ()

-- descriptions of IO actions
sayHello :: IO ()
sayHello = putStrLn "Hello!"

sayHelloTwice :: IO ()
sayHelloTwice = do
    putStrLn "Hello!"
    putStrLn "Hello again!"

doTwice :: IO a -> IO a
doTwice action = undefined

greet :: IO ()
greet = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")

getLength :: IO Int
getLength = do
    theString <- getLine
    return (length theString)

getLength' :: IO Int
getLength' = mapIO length getLine


-- do twice and return both results
doTwice' :: IO a -> IO (a, a)
doTwice' action = undefined

doForever :: IO a -> IO ()
doForever action = undefined

-- do as long as result is True
while :: IO Bool -> IO ()
while act = undefined

-- get lines until a short string is received, using while
untilShortString :: IO ()
untilShortString = undefined

-- return new IO action with function applied to result
mapIO :: (a -> b) -> IO a -> IO b
mapIO = undefined

-- instance Functor IO where
--     fmap :: (a -> b) -> IO a -> IO b
--     fmap = mapIO


-- perform only if True, otherwise no-op
doWhen :: Bool -> IO () -> IO ()
doWhen = undefined


