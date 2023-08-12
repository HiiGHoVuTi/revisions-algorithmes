module RoseTree where

data RoseTree a = NoRoses | Rose a (RoseForest a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

type RoseForest a = [RoseTree a]

unRose :: RoseTree a -> Maybe (a, RoseForest a)
unRose NoRoses = Nothing
unRose (Rose a f) = Just (a, f)
