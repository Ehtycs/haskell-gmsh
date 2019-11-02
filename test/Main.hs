module Main where

import CubeSphereTest (cubeSphereMain)
import CubeTest (cubeMain)

main = do
   putStrLn ""
   putStrLn "Running some tests..."
   cubeMain
   cubeSphereMain
