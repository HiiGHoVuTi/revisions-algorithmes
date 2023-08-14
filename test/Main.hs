{-# LANGUAGE GADTs #-}

import Control.Monad.ST
import Data.Data
import Data.Foldable (for_)
import Data.Function
import Data.List (nubBy, permutations, sort, sortOn, unfoldr)
import Data.Monoid (Sum)
import Data.STRef
import Data.Tuple
import Dijkstra
import Graph
import Levenstein
import PriorityQueue as Q
import Test.Tasty.Bench
import Test.Tasty.QuickCheck
import Trie qualified

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

correctionDijkstra :: forall w k. (Eq w, Ord w, Monoid w, Eq k, Ord k, Bounded k, Show w, Show k) => WeightedGraph k w -> k -> k -> ([k] -> Bool) -> Bool
correctionDijkstra g u v p = maybe False p (dijkstra @(SkewHeap w [k]) Proxy g u (== v))

correctionPrefixe :: [String] -> [String] -> Bool
correctionPrefixe strs' contra' =
  all (`Trie.member` arb) strs
    && not (any (`Trie.member` arb) contra)
  where
    arb = foldr Trie.insert Trie.empty strs
    strs = filter (not . null) strs'
    contra = filter (`notElem` strs) contra'

correctionAnagrammesEx :: Bool
correctionAnagrammesEx = all (`elem` ana') ana && all (`elem` ana) ana'
  where
    ana = ["pirate", "paitre", "parite", "patrie", "partie", "pretai", "repait", "etripa"]
    ana' = Trie.anagrammes "pirate" arb
    arb = foldr Trie.insert Trie.empty (ana ++ ["dodu", "etrier", "haricot", "pretre", "pretait", "patriote"])

correctionAnagrammes :: [String] -> String -> Property
correctionAnagrammes noise word = word /= "" && length word <= 6 ==> all (`elem` ana') ana && all (`elem` ana) ana'
  where
    ana = permutations word
    ana' = Trie.anagrammes word arb
    arb = foldr Trie.insert Trie.empty (ana ++ filter (not . null) noise)

main :: IO ()
main =
  defaultMain
    [ testProperty "vie est belle" True,
      bgroup
        "file de priorité"
        [ testProperty
            "correction tas d'inclinaison"
            (correctionFile @(SkewHeap Int Char) Proxy),
          testProperty
            "correction tas rouge noir"
            (correctionFile @(RBQueue Int Char) Proxy),
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
              bgroup "étalonnage tas d'inclinaison" $
                do
                  n <- [100, 1_000, 10_000, 100_000, 1_000_000]
                  let name = "n = " <> show n
                  pure $
                    bench name $
                      nf (etalonFile @(SkewHeap Int ())) n,
              bgroup "étalonnage tas arbre rouge noir" $
                do
                  n <- [100, 1_000, 10_000, 100_000, 1_000_000]
                  let name = "n = " <> show n
                  pure $
                    bench name $
                      nf (etalonFile @(RBQueue Int ())) n,
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
              bgroup "étalonnage tas d'inclinaison" $
                do
                  n <- [100, 1_000, 10_000, 100_000, 1_000_000]
                  let name = "n = " <> show n
                  pure $
                    bench name $
                      nf
                        (queueSort @(SkewHeap Integer ()) Proxy)
                        [i ^ (5 :: Integer) `mod` 35317 | i <- [1 .. n]],
              bgroup "étalonnage arbre rouge noir" $
                do
                  n <- [100, 1_000, 10_000, 100_000, 1_000_000]
                  let name = "n = " <> show n
                  pure $
                    bench name $
                      nf
                        (queueSort @(RBQueue Int ()) Proxy)
                        [i ^ (5 :: Int) `mod` 35317 | i <- [1 .. n]],
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
        ],
      bgroup
        "parcours de graphe"
        [ bgroup
            "dijkstra sur des graphes exemples"
            [ testProperty "correction dijkstra" $
                let isIncreasing [] = True
                    isIncreasing [_] = True
                    isIncreasing ((a, b) : (c, d) : rest) = a <= c && b <= d && isIncreasing ((c, d) : rest)
                 in \n m ->
                      (n >= 1 && m >= 1)
                        ==> correctionDijkstra
                          (uniformWeight (exGraphLattice n m) (1 :: Sum Int))
                          (1, 1)
                          (n, m)
                          isIncreasing
            ]
        ],
      bgroup
        "arbre préfixe"
        [ testProperty "correction arbre préfixe" correctionPrefixe,
          testProperty "correction anagrammes exemple" correctionAnagrammesEx,
          testProperty "correction anagrammes" correctionAnagrammes
        ],
      bgroup
        "algorithmes de texte"
        [ testProperty "correction Levenstein" $ \(v :: Int, w, x, y, z) gs ->
            length gs <= 20 ==> let a = [v, w, x, y, z]; b = gs in levensteinNaive a b == levenstein a b,
          bench "Levenstein naïf" $ nf (uncurry levensteinNaive) (show @[Int] [1 .. 7], show @[Int] [6 .. 11]),
          bench "Levenstein avec vecteur" $ nf (uncurry levenstein) (show @[Int] [1 .. 7], show @[Int] [6 .. 11])
        ]
    ]
