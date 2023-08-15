module Huffman (huffman) where

import BinaryTree
import Data.Bifunctor
import Map
import PriorityQueue as Q

huffman :: String -> BTree () (Char, [Bool])
huffman = labelTree . huffmanTree . occurrences

occurrences :: String -> SkewHeap Int (BTree () Char)
occurrences =
  Q.fromList
    . fmap (first BLeaf)
    . toPairs
    . Prelude.foldl (\m k -> insertModify k (maybe 1 succ) m) mempty

huffmanTree :: SkewHeap Int (BTree () Char) -> BTree () Char
huffmanTree q = case Q.deleteMin q of
  (_, Nothing) -> error "impossible"
  (q', Just (pa, a)) -> case Q.deleteMin q' of
    (_, Nothing) -> a
    (nQ, Just (pb, b)) -> huffmanTree (Q.insert (BNode () a b) (pa + pb) nQ)

labelTree :: BTree () a -> BTree () (a, [Bool])
labelTree (BLeaf c) = BLeaf (c, [])
labelTree (BNode () l r) =
  BNode
    ()
    (fmap (False :) <$> labelTree l)
    (fmap (True :) <$> labelTree r)
