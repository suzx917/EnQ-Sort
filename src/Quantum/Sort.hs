{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Quantum.Sort where

import Control.Monad
import Control.Monad.Identity

import Data.Bifunctor
import Data.Coerce

import Data.Maybe

import Data.List

import Quantum.Program hiding (Var, var)
import Quantum.ExampleData
import Quantum.DistinctDepthN

import Debug.Trace

import Prelude hiding ((<>))
import Numeric.LinearAlgebra 


combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

toIs :: Integral a => [a] -> [I]
toIs = map fromIntegral

projMat :: Integral a => [a] -> Matrix I
projMat as = col <> row -- matrix multiplication is written as <>
  where
    xs  = toIs as
    n   = length xs
    col = (n >< 1) xs
    row = (1 >< n) xs


type Constraints var val cost = [(var, val)] -> cost

matToVec :: (Numeric a) => Matrix a -> Vector a
matToVec m = flatten (tr m)

vecToMat :: (Numeric a) => Vector a -> Matrix a
vecToMat v = tr $ reshape n v
  where
    len = size v
    n = floor $ sqrt $ fromIntegral len

vec1 = range 3



-- QUBO form:
-- z* = argmin(z \in {0,1}^(n^2) ) z^T R z + r^T z
 
