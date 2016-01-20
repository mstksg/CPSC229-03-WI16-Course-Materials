
x :: IO ()
x = putStrLn "i am x"

y :: Int
y = 10

-- main :: IO ()
-- main = do
--     putStrLn "hello, what is your name?"
--     name <- getLine
--     putStrLn ("hello, " ++ name)
--     fileContents <- readFile "hello.txt"
--     putStrLn fileContents

main :: IO ()
main = do
    putStrLn "put string you want to find lenght of"
    l <- getLength'
    putStrLn ("The string has length " ++ show l)

getLength :: IO Int
getLength = do
    theString <- getLine
    return (length theString)

getLength' :: IO Int
getLength' = mapIO length getLine

mapIO :: (a -> b) -> IO a -> IO b
mapIO f action = do
    res <- action
    return (f res)



-- getLine :: IO String
--
-- readFile :: FilePath -> IO String
-- 
-- readFile "hello.txt" :: IO String

-- return :: a -> IO a
-- return x -- a no-op IO action, result is x

noOp :: IO ()
noOp = return ()

