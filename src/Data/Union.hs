module Data.Union (
    Union,
    Node (..),
    size,
    lookup,
    lookupFlattened,
) where

import Prelude hiding (lookup)
import Data.Union.Type (Union, Node (..))
import qualified Data.Union.Type as T
import Data.Array.Base ((!))

size :: Union a -> Int
size = T.size

lookup :: Union a -> Node -> (Node, a)
lookup u (Node n) = go n where
    go n | n' == n   = (Node n, T.label u ! n)
         | otherwise = go n'
      where
        n' = T.up u ! n

lookupFlattened :: Union a -> Node -> (Node, a)
lookupFlattened u (Node n) = (Node (T.up u ! n), T.label u ! n)
