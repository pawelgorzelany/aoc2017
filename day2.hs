import Data.List

-- part one
diff :: [Int] -> Int
diff line = maximum line - minimum line

-- part two
evenly :: [Int] -> Int
evenly line = head [x `div` y | x <- reverse xs, y <- xs, x `rem` y == 0, x /= y]
    where xs = sort line

checksum :: ([Int] -> Int) -> [[Int]] -> Int
checksum f input = sum $ map f input


main :: IO ()
main = do
  puzzle <- getContents
  let content = (map ((map read) . words) . lines) puzzle
  putStrLn $ "Part one " ++ show (checksum diff content)
  putStrLn $ "Part two " ++ show (checksum evenly content)
