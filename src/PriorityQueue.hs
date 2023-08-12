{-# LANGUAGE TypeFamilies #-}

module PriorityQueue where

import BinaryTree
import BinomialTree qualified as BT
import Data.Coerce
import Data.Foldable
import Data.Function
import Data.List (sortOn)
import Data.Maybe
import RoseTree

class Monoid a => PriorityQueue a where
  type Elem a
  type Priority a

  findMin :: a -> Maybe (Priority a, Elem a)
  deleteMin :: a -> (a, Maybe (Priority a, Elem a))
  insert :: Elem a -> Priority a -> a -> a
  decreasePriority :: (Elem a -> Bool) -> (Priority a -> Priority a) -> a -> a

singleton :: PriorityQueue a => Elem a -> Priority a -> a
singleton e p = insert e p mempty

peek :: PriorityQueue a => a -> Maybe (Elem a)
peek = fmap snd . findMin

peekPriority :: PriorityQueue a => a -> Maybe (Priority a)
peekPriority = fmap fst . findMin

newtype SkewHeap p e = MkSkewHeap {unSkewHeap :: NonStrictBTree (p, e)}
  deriving (Show, Eq, Functor)

instance Ord p => Semigroup (SkewHeap p e) where
  MkSkewHeap (NSBT (BLeaf ())) <> a = a
  a <> MkSkewHeap (NSBT (BLeaf ())) = a
  a@(MkSkewHeap (NSBT (BNode (p, pe) pl pr))) <> b@(MkSkewHeap (NSBT (BNode (q, _) _ _)))
    | p > q = b <> a
    | otherwise =
        MkSkewHeap . coerce $ BNode (p, pe) pr (coerce (MkSkewHeap (NSBT pl) <> b))

instance Ord p => Monoid (SkewHeap p e) where
  mempty = MkSkewHeap (NSBT (BLeaf ()))

instance Ord p => PriorityQueue (SkewHeap p e) where
  type Elem (SkewHeap p e) = e
  type Priority (SkewHeap p e) = p

  insert e p sk = coerce (BNode (p, e) (BLeaf ()) (BLeaf ())) <> sk
  findMin = root . unNSBT . unSkewHeap
  deleteMin (MkSkewHeap tree) = case tree of
    NSBT (BLeaf ()) -> (mempty, Nothing)
    NSBT (BNode a b c) -> (coerce b <> coerce c, Just a)
  decreasePriority key φ heap = MkSkewHeap $
    case unNSBT (unSkewHeap heap) of
      BLeaf () -> NSBT (BLeaf ())
      BNode (p, e) l r
        | key e -> NSBT (BNode (φ p, e) l r)
        | otherwise ->
            let l' = decreasePriority key φ (MkSkewHeap $ NSBT l)
                r' = decreasePriority key φ (MkSkewHeap $ NSBT r)
             in -- percolate-up
                NSBT $ case (peekPriority l' <= Just p, peekPriority r' <= Just p) of
                  (False, False) -> BNode (p, e) (coerce l') (coerce r')
                  (True, False) ->
                    let (a, b, c) = fromJust (unNode (coerce l'))
                     in BNode a (BNode (p, e) b c) (coerce r')
                  (False, True) ->
                    let (a, b, c) = fromJust (unNode (coerce r'))
                     in BNode a (coerce l') (BNode (p, e) b c)
                  _ -> error "impossible."

-- NOTE(Maxime): beaucoup d'invariants
newtype FibonacciHeap p e = MkFibo {unFibo :: BT.BinomialForest (p, e)}
  deriving (Eq, Show, Functor, Foldable, Traversable)

reduceForest :: Ord p => BT.BinomialForest (p, e) -> FibonacciHeap p e
-- FIXME(Maxime): le tri est de la triche
reduceForest forest = MkFibo . foldl go [] $ sortOn BT.size forest
  where
    go [] a = [a]
    go (x : xs) y
      | BT.size x /= BT.size y = y : x : xs
      | otherwise = go xs $ BT.combineSameHeight (compare `on` fst) x y

instance Ord p => Semigroup (FibonacciHeap p e) where
  MkFibo xs <> MkFibo ys = reduceForest (xs <> ys)

instance Ord p => Monoid (FibonacciHeap p e) where
  mempty = MkFibo mempty

minimumByMaybe :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumByMaybe _ [] = Nothing
minimumByMaybe f xs = Just (minimumBy f xs)

instance Ord p => PriorityQueue (FibonacciHeap p e) where
  type Elem (FibonacciHeap p e) = e
  type Priority (FibonacciHeap p e) = p

  findMin = minimumByMaybe (compare `on` fst) . fmap fst . mapMaybe (unRose . BT.unBinomial) . unFibo
  insert e p (MkFibo roses) = reduceForest (BT.singleton (p, e) : roses)

  -- TODO(Maxime):
  deleteMin = undefined
  decreasePriority = undefined
