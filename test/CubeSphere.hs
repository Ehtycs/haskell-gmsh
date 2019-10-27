{-
CubeSphere.hs
Copyright (C) 2019  Antero Marjamäki

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License ("LICENSE" file) for more details.
-}
module Main where

import GmshAPI

type DimTag = (Int,Int)

dtCube :: DimTag
dtCube = (3,2)
dtSphere :: DimTag
dtSphere = (3,1)
dtNorth :: DimTag
dtNorth = (2,3)
dtSouth :: DimTag
dtSouth = (2,5)

-- Rename Nothing to nil to shorten the piles of default argument
nil :: Maybe a
nil = Nothing

addPhys :: Int -> DimTag -> String -> IO()
addPhys etag (dim, ptag) name = do
   _ <- gmshModelAddPhysicalGroup dim [etag] $ Just ptag
   gmshModelSetPhysicalName dim ptag name

buildCubeSphere :: IO()
buildCubeSphere = do
   stag <- gmshModelOccAddSphere 0 0 0 0.3 nil nil nil nil
   ctag <- gmshModelOccAddBox (-0.5) (-0.5) (-0.5) 1 1 1 nil
   (dts, _) <- gmshModelOccFragment [(3,ctag)] [(3,stag)] nil nil nil
   let [(_,tsphere), (_,tcube)] = dts
   gmshModelOccSynchronize
   addPhys tsphere dtSphere "Sphere"
   addPhys tcube dtCube "Cube"
   addPhys 3 dtSouth "South"
   addPhys 5 dtNorth "North"
   return()

foreach :: IO [a] -> (a -> IO b) -> IO [b]
foreach action fun = do
   result <- action
   mapM fun result

-- what the result should be
resultBf :: [[[Double]]]
resultBf = [[[0.25,0.25,0.25,0.25]],[[0.25,0.25,0.25,0.25]]]

main :: IO ()
main = do
   gmshInitialize [] nil
   buildCubeSphere
   gmshModelMeshGenerate $ Just 3

   bfs <- foreach (gmshModelGetEntities $ Just 3) $ \(_,volume) -> do
      foreach (gmshModelMeshGetElementTypes (Just 3) (Just volume)) $ \etype -> do
         (intp, _) <- gmshModelMeshGetIntegrationPoints etype "Gauss1"
         (_, basis) <- gmshModelMeshGetBasisFunctions etype intp "Lagrange"
         -- maybe return a tuple of the data or make some datatype?
         return basis

   putStrLn ""
   if resultBf == bfs then do
      putStrLn "CubeSphere test: Correct result, didn't explode"
   else
      error "CubeSphere test: Wrong result!"

   -- to see and inspect the model
   --gmshFltkRun
   gmshFinalize
   return ()
