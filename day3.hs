type Point = (Int, Int)

move :: Point -> Point
move (x, y)
    | x >= y && x <= -y = (x + 1, y)
    | x >= -y && x > y = (x, y + 1)
    | x <= y && x > -y = (x - 1, y)
    | x <= -y && x <= y = (x, y - 1)
    | otherwise = (x, y)


step :: Int -> Point -> Point
step 0 point = point
step n point = step (n - 1) $ move point


dist :: Int -> Int
dist n = abs x + abs y
    where (x, y) = step (n - 1) (0, 0)


main :: IO ()
main = do
  puzzle <- getLine
  putStrLn $ "Part one " ++ show (dist (read puzzle))
  putStrLn $ "Part two " ++ "not implemented yet."
