{-
Main.hs
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

import Control.Concurrent (runInBoundThread)

import Control.Monad (forM)

import GmshAPI

dtCube = (3,2)
dtSphere = (3,1)
dtNorth = (2,3)
dtSouth = (2,5)

addPhys etag (dim, ptag) name = do
   gmshModelAddPhysicalGroup dim [etag] ptag
   gmshModelSetPhysicalName dim ptag name

buildCubeSphere :: IO()
buildCubeSphere = do
   stag <- gmshModelOccAddSphere 0 0 0 0.3 (-1) ((-pi)/2) (pi/2) (2*pi)
   ctag <- gmshModelOccAddBox (-0.5) (-0.5) (-0.5) 1 1 1 (-1)
   (dts, _) <- gmshModelOccFragment [(3,ctag)] [(3,stag)] (-1) True False
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

main :: IO ()
main = do
   gmshInitialize [] False
   buildCubeSphere
   gmshModelMeshGenerate 3

   asd <- foreach (gmshModelGetEntities 3) $ \(_,volume) -> do
      putStrLn $ "volume " ++ show volume
      foreach (gmshModelMeshGetElementTypes 3 volume) $ \etype -> do
         putStrLn $ "element type " ++ show etype
         (intp, intw) <- gmshModelMeshGetIntegrationPoints etype "Gauss1"
         putStrLn $ "intp " ++ show intp
         (ncomp, basis) <- gmshModelMeshGetBasisFunctions etype intp "Lagrange"
         -- maybe return a tuple of the data or make some datatype?
         return ncomp
   print asd

   gmshFltkRun
   gmshFinalize
   return ()
