module BinaryTree where

import Data.Bifunctor

data BTree a b = BLeaf b | BNode a (BTree a b) (BTree a b)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bifunctor BTree where
  bimap _ g (BLeaf x) = BLeaf (g x)
  bimap f g (BNode x l r) = BNode (f x) (bimap f g l) (bimap f g r)

newtype NonStrictBTree a = NSBT {unNSBT :: BTree a ()}
  deriving (Eq, Show)

instance Functor NonStrictBTree where
  fmap f (NSBT a) = NSBT (first f a)

root :: BTree a b -> Maybe a
root BLeaf {} = Nothing
root (BNode a _ _) = Just a

leaf :: a -> BTree a ()
leaf x = BNode x (BLeaf ()) (BLeaf ())

unNode :: BTree a b -> Maybe (a, BTree a b, BTree a b)
unNode BLeaf {} = Nothing
unNode (BNode a b c) = Just (a, b, c)
