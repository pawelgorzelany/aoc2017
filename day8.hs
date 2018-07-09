{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import qualified Data.Map.Strict as Map


type Register = String
data Op = Inc | Dec deriving Show
type Value = Int
type Cond = (Register, (Int -> Bool))
type Instruction = (Register, Op, Value, Cond)
type InstructionXX = (Register, Op, Value, (Register, String))
type State = Map.Map Register Value


conditionOperators = Map.fromList [ (">", (>))
                                  , ("<", (<))
                                  , (">=", (>=))
                                  , ("<=", (<=))
                                  , ("==", (==))
                                  , ("!=", (/=))
                                  ]


eval :: State -> Instruction -> State
eval s (r, op, v, cond) = if cop crv then s' else s
    where
      (cr, cop) = cond
      crv = Map.findWithDefault 0 cr s
      rv = Map.findWithDefault 0 r s
      rv' = case op of
             Inc -> rv + v
             Dec -> rv - v
      s' = Map.insert r rv' s


parse :: [String] -> Instruction
parse (r:op:v:"if":cr:cop:cv:[]) = (r, op', v', (cr, cop'))
    where
      op' = if op == "inc" then Inc else Dec
      v' = read v
      cop' = (flip $ conditionOperators Map.! cop) $ read cv


solve :: State -> [Instruction] -> Value
solve s [] = maximum $ Map.elems s
solve s (x:xs) = solve s' xs
    where
      s' = eval s x


main :: IO ()
main = do
  puzzle <- (map parse . map words . lines) `liftM` getContents
  putStrLn $ "Part one " ++ (show $ solve Map.empty puzzle)
