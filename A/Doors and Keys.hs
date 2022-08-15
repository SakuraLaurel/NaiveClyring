-- problem: https://codeforces.com/contest/1644/problem/A
-- origin: https://codeforces.com/contest/1644/submission/147279422
import Control.Monad.State (State, evalState, replicateM, state)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.List (elemIndex)

ok :: [Char] -> [Char]
ok s = if and [elemIndex p s <= elemIndex q s | (p, q) <- [('r', 'R'), ('g', 'G'), ('b', 'B')]] then "YES\n" else "No\n"

getInt :: State [Char] Int
getInt = state (first (\x -> read x :: Int) . span isDigit)

getNext :: State [Char] [Char]
getNext = state (first ok . span (/= '\n') . tail)

mainFunc :: State [Char] [[Char]]
mainFunc = do
  n <- getInt
  replicateM n getNext

-- replicateM n f = loop n
--   where loop c
--     | c <=0 = pure []
--     | otherwise = (:) <$> f <*> (loop (c-1))

main :: IO ()
main = do
  inp <- getContents
  putStr $ concat $ evalState mainFunc inp
-- The number of testcases, which is the first line, decides the length of the output.
-- So this program stops finally. However, if longer "inp" may change the outcome,
-- this program will keep receving the input.