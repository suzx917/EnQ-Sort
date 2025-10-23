{-# LANGUAGE FlexibleInstances #-}

module Quantum.SortQubo where

import Data.List

import Prelude hiding ((<>))
import Numeric.LinearAlgebra 

x = [1,3,2]
n = length x

lc , lr :: I
lc = fromIntegral n
lr = fromIntegral n

ones, oneT, matI, matCr, matCc, matN, matR, vecx, vecr :: Matrix I
ones = (n><1) $ take n [1,1..]
oneT = tr ones
matI = ident n
matCr = kronecker oneT matI
matCc = kronecker matI oneT
-- eq (22)
matR  = scale lr (tr matCr) <> matCr + scale lc (tr matCc) <> matCc
-- eq (23)
matN = kronecker matI $ ((1><n) $ map fromIntegral [1..n] :: Matrix I)
vecx = (n><1) x
vecr = scale (-1) (tr matN) <> vecx +
       scale (-2) (tr (scale lr matCr + scale lc matCc) <> ones)

{-
type Constraints var val cost = [(var, val)] -> cost
matToConstraints :: Int -> Matrix I -> Constraints Int Int Int
matToConstraints n m = costs
  where
    (rows, cols) = size m
    entries = [(i, j, atIndex m (i, j)) | i <- [0..rows-1], j <- [0..cols-1]]
    costs = map (\(i,j,v) -> (i,
-}
