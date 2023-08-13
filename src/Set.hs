{-# LANGUAGE DeriveAnyClass #-}

module Set where

import Control.DeepSeq
import Data.Foldable
import GHC.Generics

data Colour = R | B
  deriving (Eq, Ord, Enum, Generic, NFData)

data RBTree a = Nil | Node Colour (RBTree a) a (RBTree a)
  deriving (Eq, Functor, Foldable, Traversable, Generic, NFData)

root :: RBTree a -> Maybe a
root Nil = Nothing
root (Node _ _ x _) = Just x

leftmost :: RBTree a -> Maybe a
leftmost Nil = Nothing
leftmost (Node _ Nil x _) = Just x
leftmost (Node _ l _ _) = leftmost l

rightmost :: RBTree a -> Maybe a
rightmost Nil = Nothing
rightmost (Node _ _ x Nil) = Just x
rightmost (Node _ _ _ r) = rightmost r

member :: Ord a => a -> RBTree a -> Bool
member _ Nil = False
member x (Node _ l y r)
  | x == y = True
  | x < y = member x l
  | otherwise = member x r

balance :: Colour -> RBTree a -> a -> RBTree a -> RBTree a
balance B (Node R (Node R x a y) b z) c w = Node R (Node B x a y) b (Node B z c w)
balance B (Node R x a (Node R y b z)) c w = Node R (Node B x a y) b (Node B z c w)
balance B x a (Node R (Node R y b z) c w) = Node R (Node B x a y) b (Node B z c w)
balance B x a (Node R y b (Node R z c w)) = Node R (Node B x a y) b (Node B z c w)
balance c l x r = Node c l x r

makeRootBlack :: RBTree a -> RBTree a
makeRootBlack Nil = Nil
makeRootBlack (Node _ l a r) = Node B l a r

insertDup :: Ord a => a -> RBTree a -> RBTree a
insertDup x xs = makeRootBlack (go xs)
  where
    go Nil = Node R Nil x Nil
    go (Node c l y r)
      | x <= y = balance c (go l) y r
      | otherwise = balance c l y (go r)

insert :: Ord a => a -> RBTree a -> RBTree a
insert x xs = makeRootBlack (go xs)
  where
    go Nil = Node R Nil x Nil
    go (Node c l y r)
      | x == y = Node c l y r
      | x < y = balance c (go l) y r
      | otherwise = balance c l y (go r)

deleteMin :: Ord a => RBTree a -> (RBTree a, Maybe a)
deleteMin Nil = (Nil, Nothing)
deleteMin (Node _ Nil x r) = (r, Just x)
deleteMin (Node c l x r) = (balance c l' x r, out)
  where
    (l', out) = deleteMin l

instance Ord a => Semigroup (RBTree a) where
  a <> b = foldr insert a (toList b)

instance Ord a => Monoid (RBTree a) where
  mempty = Nil
