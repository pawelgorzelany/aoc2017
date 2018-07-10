import Control.Monad

type Point = (Int, Int)
type Cell = (Int, Point)

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


adjacent :: Point -> Point -> Bool
adjacent (x, y) (x', y') = not (x == x' && y == y') && dx <= 1 && dy <= 1
    where dx = abs (x' - x)
          dy = abs (y' - y)


next :: [Cell] -> [Cell]
next xs = (nx, np):xs
    where (_, pp) = head xs
          np = move pp
          nx = sum [x | (x, p) <- xs, adjacent p np]


memory :: [Cell]
memory = map head $ iterate next [(1, (0, 0))]


main :: IO ()
main = do
  puzzle <- read `liftM` getLine
  putStrLn $ "Part one " ++ show (dist puzzle)
  putStr "Part two "
  putStrLn . show . head $ take 1 [x | (x, _) <- memory, x > puzzle]
