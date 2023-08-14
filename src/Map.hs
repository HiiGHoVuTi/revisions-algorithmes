module Map where

import Control.DeepSeq
import Data.Foldable
import GHC.Generics
import Set qualified as S

newtype First a = First {getFirst :: a}
  deriving (Functor, Foldable, Traversable, Generic, NFData)

instance Eq a => Eq (First (a, b)) where
  First (a, _) == First (b, _) = a == b

instance Ord a => Ord (First (a, b)) where
  compare (First (a, _)) (First (b, _)) = compare a b

instance Bounded a => Bounded (First (a, b)) where
  maxBound = First (maxBound, undefined)
  minBound = First (minBound, undefined)

makeFirst :: a -> First (a, b)
makeFirst a = First (a, undefined)

newtype Map k v = MkMap {unMap :: S.RBTree (First (k, v))}
  deriving (Functor, Foldable, Semigroup, Monoid)

null :: Map k v -> Bool
null (MkMap S.Nil) = True
null _ = False

member :: Ord k => k -> Map k v -> Bool
member x xs = S.member (makeFirst x) (unMap xs)

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v = MkMap . S.insert (First (k, v)) . unMap

fromList :: Ord k => [(k, v)] -> Map k v
fromList = foldr (uncurry insert) mempty

toPairs :: Ord k => Map k v -> [(k, v)]
toPairs = fmap getFirst . toList . unMap

(!?) :: Ord k => Map k v -> k -> Maybe v
MkMap tree !? k = case tree of
  S.Nil -> Nothing
  S.Node _ l (First (k', v)) r
    | k == k' -> Just v
    | k < k' -> MkMap l !? k
    | otherwise -> MkMap r !? k

insertModify :: (Ord k) => k -> (Maybe v -> v) -> Map k v -> Map k v
insertModify k v (MkMap xs) = MkMap (S.makeRootBlack (go xs))
  where
    go S.Nil = S.Node S.R S.Nil (First (k, v Nothing)) S.Nil
    go (S.Node c l (First (k', v')) r)
      | k == k' = S.Node c l (First (k, v (Just v'))) r
      | k < k' = S.balance c (go l) (First (k', v')) r
      | otherwise = S.balance c l (First (k', v')) (go r)

change :: Ord k => ((k, v) -> v) -> k -> Map k v -> Map k v
change f k (MkMap tree) = MkMap $ case tree of
  S.Nil -> S.Nil
  S.Node c l x@(First (k', v)) r
    | k == k' -> S.Node c l (First (k, f (k, v))) r
    | k < k' -> S.Node c (unMap (change f k (MkMap l))) x r
    | otherwise -> S.Node c l x (unMap (change f k (MkMap r)))

-- TODO(Maxime): other helpers
