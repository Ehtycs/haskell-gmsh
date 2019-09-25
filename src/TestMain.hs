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

import GmshAPIManual

main :: IO ()
main = do
  gmshInitialize 0 [] 0
  gmshModelGeoAddPoint 0.0 0.0 0.0 1.0 1
  gmshModelGeoAddPoint 1.0 1.0 1.0 1.0 2
  gmshModelGeoSynchronize
  (dimtags, errcode) <- gmshModelGetEntities 0
  print errcode
  print dimtags
  gmshFltkRun
  --gmshFinalize
  return ()
