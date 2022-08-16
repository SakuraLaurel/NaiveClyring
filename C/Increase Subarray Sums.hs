-- problem: https://codeforces.com/contest/1644/problem/C
-- origin: https://codeforces.com/contest/1644/submission/147353835

----------------------------------------------------
-- WARNING: This program fails to pass testcases  --
--      "Time limit exceeded" on test 2           --
----------------------------------------------------

import Control.Monad.State (State, evalState, replicateM, state)
import Data.Bifunctor (first)
import Data.Char (isDigit, isSpace)
import Data.List (scanl')

mainFunc :: State [Char] [[Char]]
mainFunc = do
  t <- getInt
  replicateM t $ do
    n <- getInt
    x <- getInt
    a <- replicateM n getInt
    let prefixSums :: [Int]
        prefixSums = tail $ scanl' (+) 0 a

        rsByLen :: [Int] -- range sums
        rsByLen = [maximum [prefixSums !! (i + j) - prefixSums !! i | i <- [0 .. n - j - 1]] | j <- [1 .. n - 1]]
        xwides = zipWith (+) (scanr max (prefixSums !! (n - 1)) (0 : rsByLen)) [0, x ..]
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

-----------------------------------------------------------
-- The following function is more similar to Clyring‘s,  --
-- but is not as good as the former one                  --
-----------------------------------------------------------

-- mainFunc :: State [Char] [[Char]]
-- mainFunc = do
--   t <- getInt
--   replicateM t $ do
--     n <- getInt
--     x <- getInt
--     a <- replicateM n getInt
--     let prefixSums :: [(Int, Int)]
--         prefixSums = zip [0 .. n] $ scanl (+) 0 a

--         takeMax :: [(Int, Int)] -> [Int] -- If familiar with Ix class, better functions may be applied
--         takeMax s = [maxV $ fs i | i <- [0 .. n]]
--           where
--             is i = [i + ((2 * n - j + 3) * j) `div` 2 | j <- [0 .. n - i]]
--             fs i = [s !! j | j <- is i]
--             maxV y = maximum $ map snd y

--         rsByLen :: [Int] -- range sums
--         rsByLen = takeMax $ do
--           (i, psi) <- prefixSums
--           j <- [i .. n]
--           pure (j - i, snd (prefixSums !! j) - psi)

--         xwides = zipWith (+) (scanr1 max rsByLen) [0, x ..]
--     pure $ putInts $ scanl1 max xwides