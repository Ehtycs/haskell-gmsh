module Main where

import CubeSphereTest (cubeSphereMain)
import CubeTest (cubeMain)
import ViewTest (viewMain)

main = do
   putStrLn ""
   putStrLn "Running some tests..."
   cubeMain
   cubeSphereMain
   viewMain
