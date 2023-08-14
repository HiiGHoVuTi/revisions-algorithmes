{-# LANGUAGE ViewPatterns #-}

module Levenstein where

import Data.Vector as V

levenstein :: (Eq a) => [a] -> [a] -> Int
levenstein (fromList -> a) (fromList -> b) = lev (V.length a) (V.length b)
  where
    levVec :: Vector (Vector Int)
    levVec = fromList [fromList [lev i j | j <- [0 .. V.length b]] | i <- [0 .. V.length a]]

    lev :: Int -> Int -> Int
    lev i 0 = i
    lev 0 j = j
    lev i j
      | a ! (i - 1) == b ! (j - 1) = levVec ! (i - 1) ! (j - 1)
      | otherwise =
          1
            + Prelude.minimum
              [ levVec ! (i - 1) ! j,
                levVec ! i ! (j - 1),
                levVec ! (i - 1) ! (j - 1)
              ]

levensteinNaive :: (Eq a) => [a] -> [a] -> Int
levensteinNaive [] [] = 0
levensteinNaive [] ys = Prelude.length ys
levensteinNaive xs [] = Prelude.length xs
levensteinNaive (x : xs) (y : ys)
  | x == y = levensteinNaive xs ys
  | otherwise =
      1
        + Prelude.minimum
          [ levensteinNaive xs (y : ys),
            levensteinNaive (x : xs) ys,
            levensteinNaive xs ys
          ]
