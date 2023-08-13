module Graph where

import Control.Monad

data Graph k = MkGraph
  { neighbours :: k -> [k],
    listVertices :: [k]
  }

data WeightedGraph k w = MkWeightedGraph
  { wNeighbours :: k -> [(w, k)],
    wListVertices :: [k]
  }

uniformWeight :: Graph k -> w -> WeightedGraph k w
uniformWeight g w =
  MkWeightedGraph
    { wNeighbours = zip (repeat w) . neighbours g,
      wListVertices = listVertices g
    }

unWeight :: WeightedGraph k w -> Graph k
unWeight g =
  MkGraph
    { neighbours = fmap snd . wNeighbours g,
      listVertices = wListVertices g
    }

exGraphLattice :: Int -> Int -> Graph (Int, Int)
exGraphLattice n m =
  MkGraph
    { listVertices = (,) <$> [1 .. n] <*> [1 .. m],
      neighbours = \(i, j) -> do
        k <- [-1, 0, 1]
        l <- [-1, 0, 1]
        guard (k /= l)
        guard (i + k `notElem` [0, n + 1])
        guard (j + l `notElem` [0, m + 1])
        pure (i + k, j + l)
    }
