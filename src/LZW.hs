module LZW where

import Control.Monad
import Control.Monad.ST
import Data.Foldable (for_)
import Data.Functor
import Data.Maybe
import Data.STRef
import Map as M

compress :: String -> Int -> String -> [Integer]
compress _ _ [] = []
compress alphabet d (m0 : xs0) = runST $ do
  codes <- newSTRef mempty
  idx <- newSTRef 0
  let addCode c = do
        i <- readSTRef idx
        when (i < 2 ^ d) $ do
          modifySTRef' idx succ
          modifySTRef' codes (M.insert c i)
      getCode m = readSTRef codes <&> fromJust . (M.!? m)
      go m [] = getCode m <&> pure
      go m (x : xs) = do
        isInTable <- readSTRef codes <&> M.member (x : m)
        if isInTable
          then go (x : m) xs
          else do
            addCode (x : m)
            (:) <$> getCode m <*> go [x] xs
  for_ alphabet (addCode . pure)
  go [m0] xs0

decompress :: String -> Int -> [Integer] -> String
decompress _ _ [] = []
decompress alphabet d (m0 : xs0) = runST $ do
  codes <- newSTRef mempty
  idx <- newSTRef 0

  let addCode c = do
        i <- readSTRef idx
        when (i < 2 ^ d) $ do
          modifySTRef' idx succ
          modifySTRef' codes (M.insert i c)
      getCode m = readSTRef codes <&> fromJust . (M.!? m)
      go _ [] = pure []
      go c (n : ns) = do
        m <- getCode c
        (x : m') <- getCode n
        addCode (m <> [x])
        ((x : m') <>) <$> go n ns

  for_ alphabet (addCode . pure)
  (<>) <$> getCode m0 <*> go m0 xs0
