{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

import Control.Monad
import Data.Ord
import Data.List
import Data.Tree
import qualified Data.Map.Strict as Map

type Name = String
type Weight = Int
type Program = (Name, Weight)
type Children = [Name]


strip :: (Eq a) => a -> [a] -> [a]
strip x xs = filter (/= x) xs


weight :: String -> Int
weight = read . strip '(' . strip ')'


parse :: [String] -> (Name, (Program, Children))
parse (x:w:[]) = (x, ((x, weight w), []))
parse (x:w:"->":xs) = (x, ((x, weight w), strip ',' <$> xs))


hasChildren :: (Name, (Program, Children)) -> Bool
hasChildren (_, (_, [])) = False
hasChildren _ = True


root :: [(Name, (Program, Children))] -> Name
root programs = head $ filter (not . flip elem children) candidates
    where candidates = fst <$> filter hasChildren programs
          children = concatMap (snd . snd) programs


addWeights :: Program -> [Weight] -> Weight
addWeights p xs = snd p + sum xs


balance :: Weight -> Tree Program -> Weight
balance diff (Node (_, w) children) =
    if balanced then w + diff else balance diff' tree'
        where balanced = (==) 1 $ length $ nub folds
              folds = foldTree addWeights <$> children
              diff' = minimum folds - maximum folds
              tree' = case index of
                  Just i -> children !! i
              different = head . minimumBy (comparing length) . group
              index = elemIndex (different folds) folds


tree :: [(Name, (Program, Children))] -> Tree Program
tree programs = unfoldTree (\k -> m Map.! k) $ root programs
    where m = Map.fromList programs


main :: IO ()
main = do
  puzzle <- (map parse . map words . lines) `liftM` getContents
  putStrLn $ "Part one " ++ root puzzle
  putStrLn $ "Part two " ++ (show . balance 0 $ tree puzzle)
