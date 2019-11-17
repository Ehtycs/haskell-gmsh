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
module CubeTest (cubeMain) where

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

buildCube :: IO()
buildCube = do
   ctag <- gmshModelOccAddBox (-0.5) (-0.5) (-0.5) 1 1 1 nil
   gmshModelOccSynchronize
   addPhys 1 dtCube "Cube"
   addPhys 3 dtSouth "South"
   addPhys 5 dtNorth "North"
   return ()

foreach :: IO [a] -> (a -> IO b) -> IO [b]
foreach action fun = do
   result <- action
   mapM fun result

testCubeElementTypes :: [Int]
testCubeElementTypes = [1,2,4,15]

testCubeNumElements :: [Int]
testCubeNumElements = [12, 24, 24, 14]

cubeMain :: IO()
cubeMain = do
   gmshInitialize [] nil
   buildCube
   points <- gmshModelGetEntities (Just 0)
   gmshModelOccSetMeshSize points 1.0
   gmshModelOccSynchronize

   gmshModelMeshGenerate $ Just 3

   (etypes,etags ,_) <- gmshModelMeshGetElements Nothing Nothing
   (ntags,_,_) <- gmshModelMeshGetNodes Nothing Nothing Nothing Nothing

   let numElems = map length etags

   print etypes
   print numElems

   let tests = [ etypes == [1, 2, 4, 15]
               , numElems == [12, 24, 24, 8]
               ]

   --gmshFltkRun
   if all id tests then
      putStrLn "Cube test passed"
   else
      error "Cube test failed"

   -- gmshFltkRun
   gmshFinalize
   return ()
