{-# LANGUAGE InstanceSigs #-}

-- Equational reasoning

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- (f . g) x = f (g x)
-- id x = x

-- PROVE: f . id = f
--
-- (f . id) x = f (id x)    -- substiute (.)
--            = f x         -- subistute id
-- (f . id)   = f           -- the same
-- QED


data Option a = None | Some a
    deriving (Show, Eq)

mapOption :: (a -> b) -> Option a -> Option b
mapOption f None     = None
mapOption f (Some x) = Some (f x)

-- PROVE: mapOption id = id

-- mapOption id None = None
-- id None           = None
--
-- mapOption id (Some x) = Some (id x)
--                       = Some x
-- id (Some x)           = Some x
--
-- QED

-- PROVE: mapOption f . mapOption g = mapOption (f . g)
--
-- aka: mapOption f (mapOption g x) = mapOption (f . g) x

-- mapOption f (mapOption g None)
--   = mapOption f None
--   = None
--
-- mapOption (f . g) None
--   = None
--
-- mapOption f (mapOption g (Some x))
--   = mapOption f (Some (g x))
--   = Some (f (g x))
--
-- mapOption (f . g) (Some x)
--   = Some ((f . g) x)
--   = Some (f (g x))
--
-- QED

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

-- Imperative "chomping": (tail recursion, more performant)
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f initialAcc [] = initialAcc
foldl'' f acc (x:xs) =
    let newAcc = f acc x
    in  foldl'' f newAcc xs     -- continue choming with new accumulator

-- Functional "deconstruction": (more elegant, works with laziness)
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z                           -- replace [] with z
foldr' f z (x : xs) = x `f` foldr' f z xs   -- repliace (:) with f

anyTrue :: [Bool] -> Bool
-- anyTrue xs = foldl (||) False xs     -- doesn't work on infinite lists!
anyTrue xs = foldr (||) False xs        -- works on infinite lists!

-- | Trees

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Leaf = Leaf
    fmap f (Node x tL tR) =
         Node (f x) (fmap f tL) (fmap f tR)

-- | Binary search trees

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree x Leaf = Node x Leaf Leaf
insertTree x (Node y tL tR)
    | x < y     = Node y (insertTree x tL) tR
    | otherwise = Node y tL (insertTree x tR)

constructTree :: Ord a => [a] -> Tree a
constructTree = foldl (\acc newItem -> insertTree newItem acc) Leaf
                   -- ^ update function

isInTree :: Ord a
         => a -> Tree a -> Bool
isInTree x Leaf = False
isInTree x (Node y tL tR)
    | x == y    = True
    | x < y     = isInTree x tL
    | otherwise = isInTree x tR

sumTree :: Num a
        => Tree a -> a
sumTree Leaf           = 0
sumTree (Node x tL tR) = x + sumTree tL + sumTree tR

-- exercise:
-- remove :: Ord a => (a -> Bool) -> Tree a -> Tree a

-- | IO

-- data () = ()

-- descriptions of IO actions
sayHello :: IO ()
sayHello = putStrLn "Hello!"

sayHelloTwice :: IO ()
sayHelloTwice = do
    putStrLn "Hello!"
    putStrLn "Hello again!"

-- impossible in C, python, etc!
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
    return (res1, res2)         -- no-op


doForever :: IO a -> IO ()
doForever action = do
    action
    doForever action

while :: IO Bool -> IO ()
while act = do
    res <- act
    case res of
      True  -> while act
      False -> return ()

mapIO :: (a -> b) -> IO a -> IO b
mapIO f action = do
    res <- action
    return (f res)

untilShortString :: IO ()
untilShortString = while (mapIO (< 10) getLength)

-- instance Functor IO where
--     fmap :: (a -> b) -> IO a -> IO b
--     fmap f action = do
--       res <- action
--       return (f res)


doWhen :: Bool -> IO () -> IO ()
doWhen True  action = action        -- answer is original action
doWhen False action = return ()     -- answer is no-op

