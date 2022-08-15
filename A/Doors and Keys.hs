-- origin: https://codeforces.com/contest/1644/submission/147279422
import Control.Monad.State (State, evalState, replicateM, state)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.List (elemIndex)

ok :: [Char] -> [Char]
ok s =
  if and [elemIndex p s <= elemIndex q s | (p, q) <- [('r', 'R'), ('g', 'G'), ('b', 'B')]]
    then "YES\n"
    else "No\n"

getInt :: State [Char] Int
getInt = state func
  where
    func s = first (\x -> read x :: Int) $ span isDigit s

getNext :: State [Char] [Char]
getNext = state func
  where
    func s = first ok $ span (/= '\n') (tail s)

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