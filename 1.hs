
main = do
  contents <- getContents
  check $ map wordToInt $ words contents
  check2 $ map wordToInt $ words contents
  putStrLn "the end"

wordToInt :: [Char] -> Int
wordToInt x = read x ::Int


-- part 1

check (x:xs)
  | result > 0 = print result
  | xs == [] = return ()
  | otherwise = check xs
  where result = getResult (x:xs)

matchesTest :: Int -> Int -> Bool
matchesTest a b
  | a + b == 2020 = True
  | otherwise = False


getResult :: [Int] -> Int
getResult (x:xs)
  | xs == []  = 0
  | matchesTest x $ head xs = x * head xs
  | otherwise = getResult (x:(tail xs))

-- part 2

check2 list = print $ head [ a * b * c | a <- list, b <- list, c <- list, a + b + c == 2020 ]
