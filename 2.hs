main :: IO ()
main = do
  contents <- getContents
  validPassCount $ lines contents
  putStrLn "the end"

check :: [Char] -> Char -> Int -> Int -> Bool
check password char min max
  | counterChar >= min && counterChar <= max = True
  | otherwise = False
  where counterChar = count password char

count :: [Char] -> Char -> Int
count password char = sum [ 1 | n <- password, n == char ]

getMinMax :: String -> ([Char], [Char])
getMinMax line = break (=='-') $ head $ words line

getC :: String -> Char
getC line = head $ words line !! 1

verify :: String -> Bool
verify line = check pass char min max
  where pass = words line !! 2
        char = getC line
        min = read $ fst $ getMinMax line
        max = read $ tail $ snd $ getMinMax line

validPassCount :: [[Char]] -> IO ()
validPassCount lines = print $ sum [1 | line <- lines, verify line ]
