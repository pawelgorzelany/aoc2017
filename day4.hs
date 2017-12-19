import Data.List
import Control.Monad
import qualified Data.Set as Set


dedup :: (Ord a) => [a] -> [a]
dedup = Set.toList . Set.fromList


dups :: [String] -> Bool
dups pass = (length $ dedup pass) == length pass


anagrams :: [String] -> [String]
anagrams = concatMap $ dedup . permutations


main :: IO ()
main = do
  puzzle <- lines `liftM` getContents
  putStr "Part one "
  putStrLn $ show $ length $ filter (dups . words) puzzle
  putStr "Part two "
  putStrLn $ show $ length $ filter (dups . anagrams . words) puzzle
