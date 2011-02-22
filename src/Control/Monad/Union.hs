{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}
-- |
-- Monadic interface for creating a disjoint set data structure.
--
module Control.Monad.Union (
  UnionM,
  Union (..),
  Node,
  run,
  run',
  new,
  lookup,
  annotate,
  merge
) where

import qualified Data.Union.ST as US
import Data.Union.Type (Node (..), Union (..))

import Prelude hiding (lookup)
import Control.Monad.State
import Control.Monad.ST
import Control.Applicative
import Control.Arrow (first)

data UState s l = UState {
    next   :: !Int,
    forest :: US.UnionST s l
}

-- | Union find monad.
newtype UnionM l a = U {
    runU :: (forall s . StateT (UState s l) (ST s) a)
}

instance Monad (UnionM l) where
    return x =  U (return x)
    f >>= b = U (runU f >>= runU . b)

instance Functor (UnionM l) where
    fmap = liftM

instance Applicative (UnionM l) where
    pure = return
    (<*>) = ap

-- | Run a union find computation.
run :: UnionM l a -> a
run a = runST $ do
    u <- US.new 1 undefined
    evalStateT (runU a) UState{ next = 0, forest = u }

-- | Run a union find computation; also return the final disjoint set forest
-- for querying.
run' :: UnionM l a -> (Union l, a)
run' a = runST $ do
    u <- US.new 1 undefined
    (a, s) <- runStateT (runU a) UState{ next = 0, forest = u }
    f <- US.unsafeFreeze (forest s)
    return (f, a)

-- | Add a new node, with a given label.
new :: l -> UnionM l Node
new l = U $ do
    u <- get
    let size = US.size (forest u)
        n    = next u
    if (size <= next u) then do
        forest' <- lift $ US.grow (forest u) (2*size)
        lift $ US.annotate forest' n l
        put u{ forest = forest', next = n + 1 }
     else do
        lift $ US.annotate (forest u) n l
        put u{ next = n + 1 }
    return (Node n)

-- | Find the node representing a given node, and its label.
lookup :: Node -> UnionM l (Node, l)
lookup (Node n) = U $ do
    dsf <- gets forest
    first Node <$> lift (US.lookup dsf n)

-- | Merge two sets. The first argument is a function that takes the labels
-- of the corresponding sets' representatives and computes a new label for
-- the joined set. Returns Nothing if the given nodes are in the same set
-- already.
merge :: (l -> l -> (l, a)) -> Node -> Node -> UnionM l (Maybe a)
merge f (Node n) (Node m) = U $ do
    dsf <- gets forest
    lift $ US.merge dsf f n m

-- | Re-label a node.
annotate :: Node -> l -> UnionM l ()
annotate (Node n) l = U $ do
    dsf <- gets forest
    lift $ US.annotate dsf n l
