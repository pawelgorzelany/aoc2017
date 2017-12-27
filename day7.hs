{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

import Control.Monad


strip :: Char -> String -> String
strip x xs = filter (/= x) xs


parse :: [String] -> (String, [String])
parse (x:xs) = (x, map (strip ',') $ drop 2 xs)


hasChildren :: (String, [String]) -> Bool
hasChildren (_, []) = False
hasChildren _ = True


root :: [(String, [String])] -> String
root programs = head $ filter (not . flip elem children) candidates
    where candidates = map fst $ filter hasChildren programs
          children = concatMap snd programs


main :: IO ()
main = do
  puzzle <- (map parse . map words . lines) `liftM` getContents
  putStrLn $ "Part one " ++ root puzzle
