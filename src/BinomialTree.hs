{-# LANGUAGE DeriveAnyClass #-}

module BinomialTree
  ( BinomialTree (binomialChildren, top, size),
    BinomialForest,
    singleton,
    combineSameHeight,
  )
where

import Control.DeepSeq
import GHC.Generics

data BinomialTree a = MkBinomialTree {size :: !Int, top :: a, binomialChildren :: BinomialForest a}
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic, NFData)

type BinomialForest a = [BinomialTree a]

singleton :: a -> BinomialTree a
singleton = flip (MkBinomialTree 1) []

combineSameHeight :: (a -> a -> Ordering) -> BinomialTree a -> BinomialTree a -> BinomialTree a
combineSameHeight p at@(MkBinomialTree k a as) bt@(MkBinomialTree _ b bs)
  | a `p` b /= GT = MkBinomialTree (k + 1) a (bt : as)
  | otherwise = MkBinomialTree (k + 1) b (at : bs)
