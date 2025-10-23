module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Quantum.Examples
import Quantum.Program

main = do
  args <- getArgs
  case args of
    
     ["c","eqSum"] -> do
       print $ solveClassical ct (eqSum [1,2,3])
     ["q","eqSum"] -> do
       print $ solveQuantum (eqSum [2,3,1])
       
     ["c" , "sort4231"] -> do
       print $ solveClassical ct (sortP [4,2,3,1])
     ["q" , "sort4231"] -> do
       print $ solveQuantum (sortP [4,2,3,1])
       
     ["c", "sort231"] -> do
       print $ solveClassical ct (sortP [2,3,1])
     ["q", "sort231"] -> do
       print $ solveQuantum (sortP [2,3,1])
       
     ["q" , "sort91"] -> do
       print $ solveQuantum (sortP [9,1])
       
     _ -> do
      putStrLn "Error: Unrecognized arguments"
      exitFailure
  
