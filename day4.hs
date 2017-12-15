import Data.List


dups :: [String] -> Bool
dups pass = nub pass == pass


anagrams :: [String] -> [String]
anagrams = concatMap $ nub . permutations


main :: IO ()
main = do
  puzzle <- getContents
  let p = lines puzzle
  putStrLn $ "Part one " ++ (show $ length $ filter (dups . words) p)
  -- concise but super inefficient way to solve this
  -- this was crunching for ~5 minutes
  putStrLn $ "Part two " ++ (show $ length $ filter (dups . anagrams . words) p)
