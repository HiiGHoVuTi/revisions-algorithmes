{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module PriorityQueue where

import BinaryTree
import BinomialTree qualified as BT
import Control.DeepSeq
import Data.Coerce
import Data.Data
import Data.Foldable
import Data.Function
import Data.List (sortOn, unfoldr)
import Data.Tuple
import GHC.Generics

class Monoid a => PriorityQueue a where
  type Elem a
  type Priority a

  findMin :: a -> Maybe (Priority a, Elem a)
  deleteMin :: a -> (a, Maybe (Priority a, Elem a))
  insert :: Elem a -> Priority a -> a -> a

singleton :: PriorityQueue a => Elem a -> Priority a -> a
singleton e p = insert e p mempty

fromList :: PriorityQueue a => [(Elem a, Priority a)] -> a
fromList = foldl (\q (e, p) -> insert e p q) mempty

peek :: PriorityQueue a => a -> Maybe (Elem a)
peek = fmap snd . findMin

peekPriority :: PriorityQueue a => a -> Maybe (Priority a)
peekPriority = fmap fst . findMin

queueSort :: forall a. (PriorityQueue a, Elem a ~ ()) => Proxy a -> [Priority a] -> [Priority a]
queueSort _ = fmap fst . unfoldr (fmap swap . sequence . deleteMin @a) . fromList . zip (repeat ())

instance Ord p => PriorityQueue [(p, e)] where
  type Elem [(p, e)] = e
  type Priority [(p, e)] = p

  findMin = minimumByMaybe (compare `on` fst)
  deleteMin xs = let ys = sortOn fst xs in (tail ys, Just (head ys))
  insert e p = ((p, e) :)

newtype SkewHeap p e = MkSkewHeap {unSkewHeap :: BTree (p, e) ()}
  deriving (Show, Eq, Generic, NFData)

instance Ord p => Semigroup (SkewHeap p e) where
  MkSkewHeap (BLeaf ()) <> a = a
  a <> MkSkewHeap (BLeaf ()) = a
  a@(MkSkewHeap (BNode (p, pe) pl pr)) <> b@(MkSkewHeap (BNode (q, _) _ _))
    | p > q = b <> a
    | otherwise =
        MkSkewHeap . coerce $ BNode (p, pe) pr (coerce (MkSkewHeap pl <> b))

instance Ord p => Monoid (SkewHeap p e) where
  mempty = MkSkewHeap (BLeaf ())

instance Ord p => PriorityQueue (SkewHeap p e) where
  type Elem (SkewHeap p e) = e
  type Priority (SkewHeap p e) = p

  insert e p sk = coerce (BNode (p, e) (BLeaf ()) (BLeaf ())) <> sk
  findMin = root . unSkewHeap
  deleteMin (MkSkewHeap tree) = case tree of
    BLeaf () -> (mempty, Nothing)
    BNode a b c -> (coerce b <> coerce c, Just a)

-- NOTE(Maxime): beaucoup d'invariants,
-- implémentation peu respectueuse du réel algorithme qui demande beaucoup plus de pointeurs pour se débarrasser des logs
newtype FibonacciHeap p e = MkFibo {unFibo :: BT.BinomialForest (p, e)}
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic, NFData)

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

  findMin = minimumByMaybe (compare `on` fst) . fmap BT.top . unFibo
  insert e p (MkFibo roses) = MkFibo (BT.singleton (p, e) : roses)

  deleteMin fh =
    let min' = findMin fh
        breakMin n tree
          | fst (BT.top tree) == n = BT.binomialChildren tree
          | otherwise = [tree]
     in case min' of
          Nothing -> (mempty, Nothing)
          Just (n, _) -> (reduceForest (concatMap (breakMin n) (unFibo fh)), min')
