-- problem: https://codeforces.com/contest/1644/problem/C
-- origin: https://codeforces.com/contest/1644/submission/147353835

import Control.Monad.State (State, evalState, replicateM, state)
import Data.Array (Array, listArray, (!))
import Data.Bifunctor (first)
import Data.Char (isDigit, isSpace)

mainFunc :: State [Char] [[Char]]
mainFunc = do
  t <- getInt
  replicateM t $ do
    n <- getInt
    x <- getInt
    a <- replicateM n getInt
    let -- Start to calculate len, which is the length corresponding to the max subarray sum.
        func (startV, startI, endV, endI) (i, v) = (sv, si, ev, ei)
          where
            (sv, si) = if startV < 0 then (v, i) else (startV + v, startI)
            (ev, ei) = if sv > endV then (sv, i) else (endV, endI)

        (startV, startI, maxV, endI) = foldl func (0, 0, 0, -1) $ zip [0 ..] a
        len = let ans = endI - startI + 1 in if ans <= 0 then 0 else ans
        -- End calculating. If don't want to calculate it, set len = 0, and replace maxByLen.

        -- len = 0

        prefixSums :: Array Int Int
        prefixSums = listArray (0, n) $ scanl (+) 0 a
        rsByLen :: [Int] -- range sums
        rsByLen = [maximum [prefixSums ! (i + j) - prefixSums ! i | i <- [0 .. n - j]] | j <- [len + 1 .. n - 1]]

        maxByLen
          | len == n = replicate (len + 1) maxV
          | otherwise = replicate (len + 1) maxV ++ scanr max (prefixSums ! n) rsByLen

        -- maxByLen = scanr max (prefixSums ! n) (0:rsByLen)

        xwides = zipWith (+) maxByLen [0, x ..]
    pure $ putInts $ scanl1 max xwides

getInt :: State [Char] Int
getInt = state (first (\x -> read x :: Int) . span isInt . dropWhile isSpace)
  where
    isInt c = isDigit c || (c == '-')

putInts :: [Int] -> [Char]
putInts x = unwords (map show x) ++ "\n"

main :: IO ()
main = do
  inp <- getContents
  putStr $ concat $ evalState mainFunc inp