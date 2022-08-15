-- problem: https://codeforces.com/contest/1635/problem/A
-- origin: https://codeforces.com/contest/1635/submission/147037193

import Control.Monad.State (State, evalState, replicateM, state)
import Data.Bifunctor (first)
import Data.Bits ((.|.))
import Data.Char (isDigit, isSpace)

getInt :: State [Char] Int
getInt = state (first (\x -> read x :: Int) . span isDigit . dropWhile isSpace)

mainFunc :: State [Char] [Char]
mainFunc = do
  t <- getInt
  fmap mconcat $
    replicateM t $ do
      n <- getInt
      a <- replicateM n getInt
      pure $ show (foldl (.|.) 0 a) ++ "\n"

main :: IO ()
main = do
  inp <- getContents
  putStr $ evalState mainFunc inp