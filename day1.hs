import Data.Char

captcha :: Int -> [Char] -> Int
captcha n xs = sum [digitToInt x | (x, y) <- zip xs $ drop n (cycle xs), x == y]

-- part two
halfCaptcha :: [Char] -> Int
halfCaptcha xs = captcha n xs
                 where n = div (length xs) 2

main :: IO ()
main = do
  puzzle <- getLine
  putStrLn $ "Part one " ++ show (captcha 1 puzzle)
  putStrLn $ "Part two " ++ show (halfCaptcha puzzle)
