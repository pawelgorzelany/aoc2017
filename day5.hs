import Control.Monad
import Data.Vector (Vector, fromList, (!?), (//))


incOrDecIf :: (Int -> Bool) -> Int -> Int
incOrDecIf p x = if p x then x - 1 else x + 1


countJumps :: Vector Int -> Int -> Int -> (Int -> Int) -> Int
countJumps xs n c f = case xs !? n of
                        Nothing -> c
                        Just x -> countJumps xs' n' (c + 1) f
                            where xs' = xs // [(n, f x)]
                                  n' = x + n


main :: IO ()
main = do
  puzzle <- (fromList . map read . lines) `liftM` getContents
  putStr "Part one "
  putStrLn $ show $ countJumps `in_` puzzle `using` (+ 1)
  putStr "Part two "
  putStrLn $ show $ countJumps `in_` puzzle `using` (incOrDecIf (>= 3) )
      where a `in_` p = a p 0 0
            using = ($)
