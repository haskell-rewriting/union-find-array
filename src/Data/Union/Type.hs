{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Union.Type (
    Union (..),
    Node (..),
) where

import Data.Array.Unboxed
import Data.Array

data Union a = Union {
    size  :: !Int,
    up    :: UArray Int Int,
    label :: Array Int a
}

newtype Node = Node Int
    deriving (Eq, Ord, Ix)