module SAT where

import Data.Maybe

type Clause = [Int]

type Formula = [Clause]

pickLiteral :: [[a]] -> Maybe a
pickLiteral = listToMaybe . concat

getUnit :: [[a]] -> Maybe a
getUnit f = listToMaybe [x | [x] <- f]

unitPropagation :: Formula -> Formula
unitPropagation f = case getUnit f of
  Nothing -> f
  Just u -> simplify f u

simplify :: Formula -> Int -> Formula
simplify f u = [filter (/= -u) c | c <- f, u `notElem` c]

satisfiable :: Formula -> Bool
satisfiable f
  | null f = True
  | otherwise = case pickLiteral f of
      Nothing -> False
      Just l -> satisfiable (simplify f l) || satisfiable (simplify f (-l))