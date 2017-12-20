import Control.Monad
import Data.Set (Set, empty, insert, member, size)
import Data.Vector (Vector, (!), (//), elemIndex, indexed, fromList, maxIndex,
                    snoc, update)
import qualified Data.Vector as Vector (length, elem, empty)


updates :: Int -> Int -> Vector Int -> Vector (Int, Int)
updates 0 _ xs = indexed xs
updates x n xs = updates x' n' xs'
    where x' = x - 1
          n' = (n + 1) `mod` Vector.length xs
          xs' = xs // [(n', (xs ! n') + 1)]


redistribute :: Vector Int -> Vector Int
redistribute xs = update zeroed $ updates x n zeroed
    where zeroed = xs // [(n, 0)]
          n = maxIndex xs
          x = xs ! n


redistributions :: Vector Int -> Set (Vector Int) -> Int
redistributions xs seen =
    if xs `member` seen then size seen
    else redistributions xs' $ insert xs seen
        where xs' = redistribute xs


cycles :: Vector Int -> Vector (Vector Int) -> Maybe Int
cycles xs seen =
    if xs `Vector.elem` seen then (Vector.length seen -) <$> elemIndex xs seen
    else cycles xs' $ snoc seen xs
        where xs' = redistribute xs


main :: IO ()
main = do
  puzzle <- (fromList . map read . words) `liftM` getLine
  putStr "Part one "
  putStrLn $ show $ redistributions puzzle empty
  putStr "Part two "
  putStrLn $ show $ solve puzzle
      where solve puzzle = case cycles puzzle Vector.empty of
                             Just x -> x
                             Nothing -> 0
