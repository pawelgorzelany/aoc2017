import Data.List
import qualified Data.Set as Set


dedup :: (Ord a) => [a] -> [a]
dedup = Set.toList . Set.fromList


dups :: [String] -> Bool
dups pass = (length $ dedup pass) == length pass


anagrams :: [String] -> [String]
anagrams = concatMap $ dedup . permutations


main :: IO ()
main = do
  puzzle <- getContents
  let p = lines puzzle
  putStrLn $ "Part one " ++ (show $ length $ filter (dups . words) p)
  putStrLn $ "Part two " ++ (show $ length $ filter (dups . anagrams . words) p)
