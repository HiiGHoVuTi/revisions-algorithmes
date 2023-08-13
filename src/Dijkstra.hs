{-# LANGUAGE GADTs #-}

module Dijkstra where

import Data.Data
import Graph
import PriorityQueue
import Set

dijkstra ::
  forall pq k w.
  (Ord k, Bounded k, Ord w, Monoid w, PQ pq w [k]) =>
  Proxy pq ->
  WeightedGraph k w ->
  k ->
  (k -> Bool) ->
  Maybe [k]
dijkstra _ g start done =
  go
    (PriorityQueue.singleton [start] (mempty @w))
    (mempty @(RBTree k))
  where
    go q v = case PriorityQueue.deleteMin @pq q of
      (_, Nothing) -> Nothing
      (_, Just (_, [])) -> error "impossible"
      (q0, Just (p, u : us))
        | done u -> Just (reverse (u : us))
        | u `member` v -> go q0 v
        | otherwise -> go toVisit visited
        where
          update (p', u') = PriorityQueue.insert (u' : u : us) (p <> p')
          visited = Set.insert u v
          toVisit = foldr update q0 (wNeighbours g u)
