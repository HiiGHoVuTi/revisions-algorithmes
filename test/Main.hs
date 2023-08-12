{-# LANGUAGE GADTs #-}

import Control.Monad.ST
import Data.Data
import Data.Foldable (for_)
import Data.Function
import Data.List (nubBy, sort, sortOn, unfoldr)
import Data.STRef
import Data.Tuple
import PriorityQueue as Q
import Test.Tasty.Bench
import Test.Tasty.QuickCheck

correctionFile ::
  forall a.
  (Ord (Priority a), Eq (Elem a), PriorityQueue a, Show (Priority a), Show (Elem a)) =>
  Proxy a ->
  [(Elem a, Priority a)] ->
  Bool
correctionFile _ input' = qSort == normalSort
  where
    input = nubBy ((==) `on` snd) input'
    normalSort = fmap swap (sortOn snd input)
    q = Q.fromList @a input
    qSort =
      unfoldr (fmap swap . sequence . Q.deleteMin) q

etalonFile ::
  forall a.
  ( PriorityQueue a,
    Ord (Priority a),
    Elem a ~ (),
    Priority a ~ Int
  ) =>
  Int ->
  a
etalonFile n = runST $ do
  q <- newSTRef (mempty @a)
  for_ [0 .. n] $ \i -> do
    writeSTRef q . insert () ((i * i * i) `mod` 3037) =<< readSTRef q
  for_ [0 .. 2 * n] $ \i -> do
    q0 <- readSTRef q
    let q1 = fst (deleteMin q0)
        q2 = insert () ((i * i * i) `mod` 2377) q1
        q3 = if i `mod` 11 == 0 then fst (deleteMin q2) else q2
    writeSTRef q q3
  readSTRef q

main :: IO ()
main =
  defaultMain
    [ testProperty "vie est belle" True,
      bgroup
        "file de priorité"
        [ testProperty
            "correction SkewHeap"
            (correctionFile @(SkewHeap Int Char) Proxy),
          testProperty
            "correction tas de Fibonacci"
            (correctionFile @(FibonacciHeap Int Char) Proxy),
          bgroup
            "insertion & retrait"
            [ bgroup "référence liste triée" $
                do
                  n <- [100, 1_000, 10_000]
                  let name = "n = " <> show n
                  pure $
                    bench name $
                      nf (etalonFile @[(Int, ())]) n,
              bgroup "étalonnage tas SkewHeap" $
                do
                  n <- [100, 1_000, 10_000, 100_000, 1_000_000]
                  let name = "n = " <> show n
                  pure $
                    bench name $
                      nf (etalonFile @(SkewHeap Int ())) n,
              bgroup "étalonnage tas de Fibonacci" $
                do
                  n <- [100, 1_000, 10_000, 100_000, 1_000_000]
                  let name = "n = " <> show n
                  pure $
                    bench name $
                      nf (etalonFile @(FibonacciHeap Int ())) n
            ],
          bgroup
            "tri par tas"
            [ bgroup "référence liste triée" $
                do
                  n <- [100, 1_000, 10_000, 100_000, 1_000_000]
                  let name = "n = " <> show n
                  pure $
                    bench name $
                      nf
                        sort
                        [i ^ (5 :: Integer) `mod` 35317 :: Integer | i <- [1 .. n]],
              bgroup "étalonnage tas SkewHeap" $
                do
                  n <- [100, 1_000, 10_000, 100_000, 1_000_000]
                  let name = "n = " <> show n
                  pure $
                    bench name $
                      nf
                        (queueSort @(SkewHeap Integer ()) Proxy)
                        [i ^ (5 :: Integer) `mod` 35317 | i <- [1 .. n]],
              bgroup "étalonnage tas de Fibonacci" $
                do
                  n <- [100, 1_000, 10_000, 100_000, 1_000_000]
                  let name = "n = " <> show n
                  pure $
                    bench name $
                      nf
                        (queueSort @(FibonacciHeap Integer ()) Proxy)
                        [i ^ (5 :: Integer) `mod` 35317 | i <- [1 .. n]]
            ]
        ]
    ]
