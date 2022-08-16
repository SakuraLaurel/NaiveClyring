-- problem: https://codeforces.com/contest/1644/problem/B
-- origin: https://codeforces.com/contest/1644/submission/147286507

import Control.Monad.State (State, evalState, replicateM, state)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isDigit, isSpace)
import Data.List (permutations)

isOk :: [Int] -> Bool
isOk li@(a : b : c) = and $ zipWith3 (\x y z -> x + y /= z) li (b : c) c
isOk _ = error "Wrong"

getInt :: State [Char] Int
getInt = state (first (\x -> read x :: Int) . span isDigit . dropWhile isSpace)

putInts :: [Int] -> [Char]
putInts [] = "\n"
putInts (x : xs) = show x ++ concatMap (\y -> ' ' : show y) xs ++ "\n"

mainFunc :: State [Char] [[Char]]
mainFunc = do
  t <- getInt
  replicateM t $ do
    n <- getInt
    let ans = filter isOk $ permutations [1 .. n]
        ans' = take n ans -- accelerate calculating
    pure $ concatMap putInts ans'

main :: IO ()
main = do
  inp <- getContents
  putStr $ concat $ evalState mainFunc inp
