{-# LANGUAGE InstanceSigs #-}

-- | Inside contexts

import qualified Data.Map as M
import Control.Applicative
import Control.Monad

-- data Maybe a = Nothing | Just a
--     deriving (Show, Eq)

safeDivide :: Int -> Int -> Maybe Int
safeDivide x 0 = Nothing
safeDivide x y = Just (x `div` y)

divideExact :: Int -> Int -> Maybe Int
divideExact x 0 = Nothing
divideExact x y | x `mod` y == 0 = Just (x `div` y)
                | otherwise      = Nothing

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

addThree :: Int -> Int
addThree = (+3)

square :: Int -> Int
square = (^ 2)

showInt :: Int -> String
showInt = show

certaintify :: Maybe a -> a
certaintify (Just x) = x
certaintify Nothing  = error "nothing there"

certaintifyDefault :: a -> Maybe a -> a
certaintifyDefault _ (Just x) = x
certaintifyDefault z Nothing  = z

data Person = P { personName :: String
                , personAge  :: Int
                } deriving (Show, Eq)

people :: M.Map Int Person
people = M.fromList [(1, P "Justin" 25)
                    ,(2, P "Gloria" 27)
                    ,(3, P "Robert" 21)
                    ,(4, P "Kaylee" 23)
                    ,(5, P "Arnold" 21)
                    ]


personFromID :: Int -> Maybe Person
personFromID i = M.lookup i people

applyInsideMaybe :: (a -> b) -> Maybe a -> Maybe b
applyInsideMaybe f Nothing = Nothing
applyInsideMaybe f (Just x) = Just (f x)

ageFromID :: Int -> Maybe Int
ageFromID i = applyInsideMaybe personAge (personFromID i)

equalAgesFromID :: Int -> Int -> Maybe Bool
equalAgesFromID x y =
    let age1 = fmap personAge (personFromID x) :: Maybe Int
        age2 = fmap personAge (personFromID y) :: Maybe Int
    in  liftA2 (==) age1 age2

-- fmap  :: (a -> b)       -> Maybe a -> Maybe b
-- (=<<) :: (a -> Maybe b) -> Maybe a -> Maybe b

data Errorable a = Error String | Success a
                 deriving (Show, Eq)

safeDivideE :: Int -> Int -> Errorable Int
safeDivideE x 0 = Error "no division by zero!"
safeDivideE x y = Success (x `div` y)

divideExactE :: Int -> Int -> Errorable Int
divideExactE x 0 = Error "no division by zero!"
divideExactE x y | x `mod` y == 0 = Success (x `div` y)
                 | otherwise      = Error "not exact"

safeHeadE :: [a] -> Errorable a
safeHeadE [] = Error "empty list!"
safeHeadE (x:_) = Success x

maybeToE :: Maybe a -> Errorable a
maybeToE Nothing = Error "it was nothing"
maybeToE (Just x) = Success x

handleError :: (String -> a) -> Errorable a -> a
handleError handler (Error err) = handler err
handleError _       (Success x) = x

instance Functor Errorable where
    fmap :: (a -> b) -> Errorable a -> Errorable b
    fmap f (Error err) = Error err
    fmap f (Success x) = Success (f x)

instance Applicative Errorable where
    pure :: a -> Errorable a
    pure = return
    (<*>) :: Errorable (a -> b) -> Errorable a -> Errorable b
    (<*>) = ap

instance Monad Errorable where
    return :: a -> Errorable a
    return x = Success x
    (>>=) :: Errorable a -> (a -> Errorable b) -> Errorable b
    Error err >>= f = Error err
    Success x >>= f = f x

data Expr = I Int
          | B Bool
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

evalIntExpr :: Expr -> Int
evalIntExpr e =
  case e of
    I i -> i
    B b -> 0
    Add e1 e2 -> evalIntExpr e1 + evalIntExpr e2
    Mul e1 e2 -> evalIntExpr e1 * evalIntExpr e2
    Div e1 e2 -> evalIntExpr e1 `div` evalIntExpr e2

-- evalIntExprE :: Expr -> Errorable Int
-- evalIntExprE e =
--   case e of
--     I i -> Success i
--     B b -> Error "it's a bool!"
--     Add e1 e2 -> evalIntExprE e1 >>= \res1 ->
--                    evalIntExprE e2 >>= \res2 ->
--                      Success (res1 + res2)
--     Mul e1 e2 -> evalIntExprE e1 >>= \res1 ->
--                    evalIntExprE e2 >>= \res2 ->
--                      Success (res1 * res2)
--     Div e1 e2 -> evalIntExprE e1 >>= \res1 ->
--                    evalIntExprE e2 >>= \res2 ->
--                      if res2 == 0
--                        then Error "divide by zero!"
--                        else Success (res1 `div` res2)

evalIntExprE :: Expr -> Errorable Int
evalIntExprE e =
  case e of
    I i -> Success i
    B b -> Error "it's a bool!"
    -- Add e1 e2 ->
    --   evalIntExprE e1 >>= \res1 ->
    --   evalIntExprE e2 >>= \res2 ->
    --   Success (res1 + res2)
    Add e1 e2 -> do
      res1 <- evalIntExprE e1
      res2 <- evalIntExprE e2
      Success (res1 + res2)
    Mul e1 e2 -> do
      res1 <- evalIntExprE e1
      res2 <- evalIntExprE e2
      Success (res1 * res2)
    Div e1 e2 -> do
      res1 <- evalIntExprE e1
      res2 <- evalIntExprE e2
      if res2 == 0
        then Error "divide by zero!"
        else Success (res1 + res2)

-- do notation

-- foo = do
--   x
--   y
--
-- foo = x >>= \_ -> y
--
-- bar = do
--   res <- x
--   y res
--
-- bar = x >>= \res -> y res

data Future r a = F (r -> a)

futureLength :: Future [a] Int
futureLength = undefined

futureHead :: Future [a] a
futureHead = undefined

instance Functor (Future r) where
    fmap :: (a -> b) -> (Future r) a -> (Future r) b
    fmap = undefined

instance Applicative (Future r) where
    pure :: a -> (Future r) a
    pure = undefined
    (<*>) :: (Future r) (a -> b) -> (Future r) a -> (Future r) b
    (<*>) = undefined

instance Monad (Future r) where
    return :: a -> Future r a
    return = pure
    (>>=) :: Future r a -> (a -> Future r b) -> Future r b
    (>>=) = undefined

-- IO!

data Logged a = L [String] a
              deriving (Show, Eq)

logDouble :: Int -> Logged Int
logDouble x = L ["I doubled something"] (x * 2)

logSquare :: Int -> Logged Int
logSquare x = L ["I squared something!"] (x ^ 2)

instance Functor Logged where
    fmap :: (a -> b) -> Logged a -> Logged b
    fmap f (L log x) = L log (f x)

instance Applicative Logged where
    pure :: a -> Logged a
    pure = return
    (<*>) :: Logged (a -> b) -> Logged a -> Logged b
    (<*>) = ap

instance Monad Logged where
    return :: a -> Logged a
    return x = L [] x
    (>>=) :: Logged a -> (a -> Logged b) -> Logged b
    L log x >>= f = let L log2 y = f x
                    in  L (log ++ log2) y
    -- L _ x >>= f = f x

-- foo =
--     logDouble 10 >>= \x ->
--     logSquare x >>= \y ->
--     L ["i'm done"] (x + y)

foo = do
    x <- logDouble 10       -- x = 20
    y <- logSquare x        -- y = 400
    L ["i'm done"] (x + y)  -- res = 420

-- Traversable

-- sequence :: [IO a] -> IO [a]


putStrLnIf :: Maybe String -> IO ()
putStrLnIf (Just x) = putStrLn x
putStrLnIf Nothing  = return ()

-- instance Traversable Maybe where
--     mapM f Nothing = return Nothing
--     mapM f (Just x) = do
--       xRes <- x
--       return (Just xRes)


