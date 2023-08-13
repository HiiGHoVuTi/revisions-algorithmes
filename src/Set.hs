module Set where

import Control.DeepSeq
import Data.Coerce
import Data.FingerTree
import Data.Foldable
import Data.Semigroup (Max (..))
import GHC.Generics

newtype SetElem a = SetElem {unSetElem :: a}
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, NFData)

newtype FingerSet a = MkFingerSet {unFingerSet :: FingerTree (Max a) (SetElem a)}
  deriving (Show, Generic)

instance (Ord a, Bounded a) => Measured (Max a) (SetElem a) where
  measure = coerce

toList' :: FingerSet a -> [SetElem a]
toList' (MkFingerSet xs) = toList xs

instance (Ord a, Bounded a) => Semigroup (FingerSet a) where
  -- FIXME(Maxime): wrong complexity
  a <> b = foldr insert a (coerce <$> toList' b)

instance (Ord a, Bounded a) => Monoid (FingerSet a) where
  mempty = MkFingerSet empty

singleton :: (Ord a, Bounded a) => a -> FingerSet a
singleton a = MkFingerSet (Data.FingerTree.singleton (coerce a))

insert :: (Ord a, Bounded a) => a -> FingerSet a -> FingerSet a
insert x set@(MkFingerSet xs)
  | x `member` set = set
  | otherwise = MkFingerSet $ l >< Data.FingerTree.singleton (SetElem x) >< r
  where
    (l, r) = split (>= Max x) xs

unsafeInsert :: (Ord a, Bounded a) => a -> FingerSet a -> FingerSet a
unsafeInsert x (MkFingerSet xs) = MkFingerSet $ l >< Data.FingerTree.singleton (SetElem x) >< r
  where
    (l, r) = split (>= Max x) xs

member :: (Ord a, Bounded a) => a -> FingerSet a -> Bool
member x (MkFingerSet xs) = case viewl (snd (split (>= Max x) xs)) of
  y :< _ | SetElem x == y -> True
  _ -> False

delete :: (Ord a, Bounded a) => a -> FingerSet a -> FingerSet a
delete x (MkFingerSet xs) = MkFingerSet $
  case search (const (>= Max x)) xs of
    Position l y r | SetElem x == y -> l >< r
    _ -> xs
