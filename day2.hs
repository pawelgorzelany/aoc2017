import Control.Monad
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
  puzzle <- (map ((map read) . words) . lines) `liftM` getContents
  putStrLn $ "Part one " ++ show (checksum diff puzzle)
  putStrLn $ "Part two " ++ show (checksum evenly puzzle)
