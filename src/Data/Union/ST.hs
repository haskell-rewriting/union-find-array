{-# LANGUAGE RankNTypes #-}
module Data.Union.ST (
    UnionST,
    runUnionST,
    new,
    grow,
    lookup,
    annotate,
    merge,
    flatten,
    size,
) where

import qualified Data.Union.Type as U

import Prelude hiding (lookup)
import Control.Monad.ST
import Control.Monad
import Control.Applicative
import Data.Array.Base
import Data.Array.ST

data UnionST s a = UnionST {
    up :: STUArray s Int Int,
    rank :: STUArray s Int Int,
    label :: STArray s Int a,
    size :: !Int,
    def :: a
}

instance Applicative (ST s) where
    (<*>) = ap
    pure = return

runUnionST :: (forall s. ST s (UnionST s a)) -> U.Union a
runUnionST a = runST a' where
    a' = do
        u <- a
        U.Union (size u) <$> unsafeFreeze (up u) <*> unsafeFreeze (label u)

new :: Int -> a -> ST s (UnionST s a)
new size def = do
    up <- newListArray (0, size-1) [0..]
    rank <- newArray (0, size-1) 0
    label <- newArray (0, size-1) def
    return UnionST{ up = up, rank = rank, label = label, size = size, def = def }

grow :: UnionST s a -> Int -> ST s (UnionST s a)
grow u size' | size' < size u = return u
grow u size' = do
    up' <- newListArray (0, size'-1) [0..]
    rank' <- newArray (0, size'-1) 0
    label' <- newArray (0, size'-1) (def u)
    forM_ [0..size u - 1] $ \i -> do
        readArray (up u) i >>= writeArray up' i
        readArray (rank u) i >>= writeArray rank' i
        readArray (label u) i >>= writeArray label' i
    return u{ up = up', rank = rank', label = label', size = size' }

annotate :: UnionST s a -> Int -> a -> ST s ()
annotate u i v = writeArray (label u) i v

lookup' :: UnionST s a -> Int -> ST s Int
lookup' u i = do
    i' <- readArray (up u) i
    if i == i' then return i else do
        i'' <- lookup' u i'
        writeArray (up u) i i''
        return i''

lookup :: UnionST s a -> Int -> ST s (Int, a)
lookup u i = do
    i' <- lookup' u i
    v' <- readArray (label u) i'
    return (i', v')

equals :: UnionST s a -> Int -> Int -> ST s Bool
equals u a b = do
    a' <- lookup' u a
    b' <- lookup' u b
    return (a' == b')

merge :: UnionST s a -> (a -> a -> (a, b)) -> Int -> Int -> ST s (Maybe b)
merge u f a b = do
    (a', va) <- lookup u a
    (b', vb) <- lookup u b
    if a' == b' then return Nothing else do
        ra <- readArray (rank u) a'
        rb <- readArray (rank u) b'
        let cont x vx y vy = do
                writeArray (label u) y (error "invalid entry")
                let (v, w) = f vx vy
                writeArray (label u) x v
                return (Just w)
        case ra `compare` rb of
            LT -> do
                writeArray (up u) a' b'
                cont b' vb a' va
            GT -> do
                writeArray (up u) b' a'
                cont a' va b' vb
            EQ -> do
                writeArray (up u) a' b'
                writeArray (rank u) b' (ra + 1)
                cont b' vb a' va

flatten :: UnionST s a -> ST s ()
flatten u = forM_ [0..size u - 1] $ lookup' u
