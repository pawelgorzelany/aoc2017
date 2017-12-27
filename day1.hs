import Data.Char

captcha :: Int -> [Char] -> Int
captcha n xs = sum [digitToInt x | (x, y) <- zip xs $ drop n (cycle xs), x == y]


main :: IO ()
main = do
  puzzle <- getLine
  putStrLn $ "Part one " ++ show (captcha 1 puzzle)
  putStrLn $ "Part two " ++ show (captcha (length puzzle `div` 2) $ puzzle)
