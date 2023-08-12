module BinomialTree
  ( BinomialTree (unBinomial, size),
    BinomialForest,
    singleton,
    combineSameHeight,
  )
where

import RoseTree

data BinomialTree a = MkBinomialTree {size :: !Int, unBinomial :: RoseTree a}
  deriving (Eq, Show, Functor, Foldable, Traversable)

type BinomialForest a = [BinomialTree a]

singleton :: a -> BinomialTree a
singleton = MkBinomialTree 1 . flip Rose []

combineSameHeight :: (a -> a -> Ordering) -> BinomialTree a -> BinomialTree a -> BinomialTree a
combineSameHeight _ (MkBinomialTree k NoRoses) _ = MkBinomialTree k NoRoses
combineSameHeight p (MkBinomialTree k (Rose a as)) (MkBinomialTree _ (Rose b bs))
  | a `p` b /= GT = MkBinomialTree (k + 1) (Rose a (Rose b bs : as))
  | otherwise = MkBinomialTree (k + 1) (Rose b (Rose a as : bs))
combineSameHeight _ _ _ = undefined
