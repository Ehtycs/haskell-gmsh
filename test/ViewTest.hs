{-
ViewTest.hs
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
module ViewTest (viewMain) where

import GmshAPI

type DimTag = (Int,Int)

dtRectangle :: DimTag
dtRectangle = (3,2)


-- Rename Nothing to nil to shorten the piles of default argument
nil :: Maybe a
nil = Nothing

addPhys :: Int -> DimTag -> String -> IO()
addPhys etag (dim, ptag) name = do
   _ <- gmshModelAddPhysicalGroup dim [etag] $ Just ptag
   gmshModelSetPhysicalName dim ptag name

buildSquare :: IO()
buildSquare = do
   ctag <- gmshModelOccAddRectangle (-0.5) (-0.5) 0 1 1 nil nil
   gmshModelOccSynchronize
   addPhys 1 dtRectangle "Rectangle"
   return ()

foreach :: IO [a] -> (a -> IO b) -> IO [b]
foreach action fun = do
   result <- action
   mapM fun result

viewMain :: IO()
viewMain = do
   gmshInitialize [] nil
   gmshClear
   gmshModelAdd "Moi"
   buildSquare
   points <- gmshModelGetEntities (Just 0)
   gmshModelOccSetMeshSize points 1.0
   gmshModelOccSynchronize

   gmshModelMeshGenerate $ Just 2

   let field = map (:[]) [1,2,3,4,5]
   view <- gmshViewAdd "View1" Nothing
   gmshViewAddModelData view 0 "" "NodeData" [1,2,3,4,5] field nil (Just 1) nil
   daatta <- gmshViewGetModelData view 0
   print field
   --gmshFltkRun
   if daatta /= ("NodeData", [1,2,3,4,5], field, 0, 1)
      then error "ViewTest failed"
      else print "ViewTest Ok"

   --gmshFltkRun
   gmshFinalize
   putStrLn "View test ran without explosions"
   return ()
