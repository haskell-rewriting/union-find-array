{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}
module Data.Union.Monad (
  UnionM,
  Node,
  run,
  new,
  lookup,
  annotate,
  merge
) where

import qualified Data.Union.ST as US
import qualified Data.Union.Type (Node)

import Prelude hiding (lookup)
import Control.Monad.State
import Control.Monad.ST
import Control.Applicative
import Control.Arrow (first)

newtype Node = Node Int
    deriving (Eq, Ord, Show)

data UState s l = UState {
    next   :: !Int,
    forest :: US.UnionST s l
}

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

run :: UnionM l a -> a
run a = runST $ do
    u <- US.new 1 undefined
    evalStateT (runU a) UState{ next = 0, forest = u }

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

lookup :: Node -> UnionM l (Node, l)
lookup (Node n) = U $ do
    dsf <- gets forest
    first Node <$> lift (US.lookup dsf n)

merge :: (l -> l -> (l, a)) -> Node -> Node -> UnionM l (Maybe a)
merge f (Node n) (Node m) = U $ do
    dsf <- gets forest
    lift $ US.merge dsf f n m

annotate :: Node -> l -> UnionM l ()
annotate (Node n) l = U $ do
    dsf <- gets forest
    lift $ US.annotate dsf n l
