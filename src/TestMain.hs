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

import GmshAPI

main :: IO ()
main = do
   putStrLn "Moi"
--  gmshInitialize [] False
--  gmshModelOccAddPoint 0.0 0.0 0.0 1.0 1
--  gmshModelOccAddPoint 1.0 1.0 1.0 1.0 2
--  gmshModelOccAddDisk 0.0 0.0 0.0 1.0 1.0 1
--  gmshModelOccAddDisk 0.5 0.0 0.0 1.0 1.0 2
--  gmshModelOccSynchronize
--  (outDimTags, outDimTagsMap) <- gmshModelOccFuse [(2,1)] [(2,2)] (-1) (-1) (-1)
--  print outDimTags
--  print outDimTagsMap
--  gmshModelOccSynchronize
--  dimtags <- gmshModelGetEntities 0

--  gmshFltkRun
  --gmshFinalize
   return ()
