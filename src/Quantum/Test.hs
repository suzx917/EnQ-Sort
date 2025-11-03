{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Woverlapping-patterns -Wincomplete-patterns #-}


-- This module is for inspecting solveQuantum function
module Quantum.Test where

import Control.Monad.State
import Control.Monad

import Data.Functor
import Data.Coerce

import Data.Foldable
import Data.List (partition, intersperse)

import Numeric.LinearAlgebra hiding ((<>), toList, scale, add)
import Data.Bifunctor (first, second)

import Data.Bits (testBit)

import Quantum.DistinctDepthN

import qualified Data.Map.Strict as M

import Quantum.Program
import Quantum.Examples hiding (var)

prog = sortP [2,1,3]

varStruct = runFresh (genChoices (struct prog))

--pairs :: [t (Var a)]
pairs = distinctNTuples (view prog)
                        varStruct

--actualTuples :: [t (Var a, b)]
actualTuples = assignChoices (choices prog) pairs
           

encodedChoices = encodeChoices (choices prog)

--decode :: (Var a, b) -> Tensor (Summed ScaledPauli)
decode (x, c) =
  decodeChoice encodedChoices c (var x)
{-
optimize :: forall x. (ShowParens x, Ord x) => Summed (Scaled x) -> Summed (Scaled x)
optimize x =
  let y = combine x
      y' = combine' x
  in
  if debugSolver && y /= y'
    then error $ "combine incorrect: " ++ show (y, y')
    else clean y
-}
--constraintResults :: [(c, t (Var a, b))]
constraintResults =
  map (\x -> (constraints prog (fmap (first choice) x), x))
      actualTuples


decodeAndDistribute = 
  fmap (\(x, varChoices) ->
           (x, fmap (fmap floatScalars . distr . decode) varChoices)) constraintResults
{-
combineSums ::
  [Summed (Scaled (Tensor PauliExpr))] ->
  Summed (Scaled (Tensor PauliExpr))
combineSums = joinSummed . Summed

applyScaling ::
  [(Complex Double, Summed (Scaled (Tensor PauliExpr)))] ->
  [Summed (Scaled (Tensor PauliExpr))]
applyScaling = map (\(k, x) -> fmap (scale k) x)

coeffsToComplex ::
  [(c, Summed (Scaled (Tensor PauliExpr)))] ->
  [(Complex Double, Summed (Scaled (Tensor PauliExpr)))]
coeffsToComplex = map (first toComplex')

commuteTensorScaling ::
  [(c, Summed (Tensor (Scaled (Tensor PauliExpr))))] ->
  [(c, Summed (Scaled (Tensor PauliExpr)))]
commuteTensorScaling = map (second (fmap commuteScaledTensor))

buildTensor ::
  [(c, t (Summed (Scaled (Tensor PauliExpr))))] ->
  [(c, Tensor (Summed (Scaled (Tensor PauliExpr))))]
buildTensor = map (second (Tensor . toList))


distributeSummedTensor ::
  [(c, Tensor (Summed (Scaled (Tensor PauliExpr))))] ->
  [(c, Summed (Tensor (Scaled (Tensor PauliExpr))))]
distributeSummedTensor = map (second distr)
-}
