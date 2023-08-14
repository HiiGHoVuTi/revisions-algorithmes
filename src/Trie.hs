module Trie where

import Data.Maybe
import GHC.Natural
import Map (Map, toPairs)
import Map qualified

newtype Trie a = MkTrie {unTrie :: Map a (TrieNode a)}

data TrieNode a = TrieNode CanEnd a (Map a (TrieNode a))

data CanEnd = YesEnd | NoEnd
  deriving (Show, Eq)

instance Semigroup CanEnd where
  NoEnd <> NoEnd = NoEnd
  _ <> _ = YesEnd

empty :: Ord a => Trie a
empty = MkTrie mempty

insert :: forall a. Ord a => [a] -> Trie a -> Trie a
insert word (MkTrie node) = MkTrie (insert' word node)
  where
    insert' :: [a] -> Map a (TrieNode a) -> Map a (TrieNode a)
    insert' [] = error "insert"
    insert' [x] = Map.insertModify x (end x)
    insert' (x : xs) = Map.insertModify x (middle xs)

    end _ (Just (TrieNode _ x m)) = TrieNode YesEnd x m
    end a _ = TrieNode YesEnd a mempty

    middle xs mn = TrieNode e x (insert' xs m)
      where
        TrieNode e x m = fromMaybe (TrieNode NoEnd x mempty) mn

member :: Ord a => [a] -> Trie a -> Bool
member [] _ = False
member (a : as) tr = case unTrie tr Map.!? a of
  Nothing -> False
  Just tn -> member' as tn
  where
    member' [] (TrieNode e _ _) = e == YesEnd
    member' (x : xs) (TrieNode _ _ m) = case m Map.!? x of
      Nothing -> False
      Just tn -> member' xs tn

anagrammes :: Ord a => [a] -> Trie a -> [[a]]
anagrammes word tr0 = go tr0 occ0 NoEnd (sum occ0)
  where
    occ0 = foldr (`Map.insertModify` maybe (1 :: Natural) succ) mempty word

    go _ _ NoEnd 0 = []
    go _ _ YesEnd 0 = [[]]
    go tr occ _ n = concatMap explore (toPairs occ)
      where
        explore (_, 0) = []
        explore (k, _) = case unTrie tr Map.!? k of
          Nothing -> []
          Just (TrieNode e _ tm) ->
            fmap
              (k :)
              (go (MkTrie tm) (Map.change (subtract 1 . snd) k occ) e (n - 1))
