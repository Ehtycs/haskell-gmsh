{-
GmshAPI.chs - c2hs definitions of the GMSH C API.
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

module GmshAPIManual where

import Control.Monad (liftM)
import Control.Applicative ((<$>))
import Foreign.C -- get the C types
import Foreign.Ptr (Ptr,nullPtr)
import Foreign.C.String (withCString)
import Foreign.Marshal (alloca)
import Foreign.Marshal.Utils (withMany)
import Foreign.Marshal.Array (withArray, peekArray)
import Foreign.Storable (peek)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

--import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V


--include "gmshc.h"

toInt :: Ptr CInt -> IO Int
toInt = liftM fromIntegral . peek

-- marshaller for argv type "char ** argv"
argv :: [String] -> (Ptr CString -> IO a) -> IO a
argv ss f = withMany withCString ss f'
   where
      -- my brain hurts but at least I can hear colors now
      f' x = withArray x f

gmshInitialize :: Int -> [String] -> Int -> IO(Int)
gmshInitialize argc ss readConfigFiles = do
  let argc' = fromIntegral argc
  let readConfigFiles' = fromIntegral readConfigFiles
  argv ss $ \argv' -> do
    alloca $ \errptr -> do
      cgmshInitialize argc' argv' readConfigFiles' errptr
      errcode <- peek errptr
      return $ fromIntegral errcode

foreign import ccall unsafe "gmshc.h gmshInitialize"
  cgmshInitialize :: CInt
                  -> (Ptr CString)
                  -> CInt
                  -> Ptr CInt
                  -> IO()

gmshModelGeoAddPoint :: Double -> Double -> Double -> Double -> Int -> IO(Int)
gmshModelGeoAddPoint x y z c tag = do
  let x' = realToFrac x
  let y' = realToFrac y
  let z' = realToFrac z
  let c' = realToFrac c
  let tag' = fromIntegral tag
  alloca $ \errptr -> do
    cgmshModelGeoAddPoint x' y' z' c' tag' errptr
    errcode <- peek errptr
    return $ fromIntegral errcode

foreign import ccall unsafe "gmshc.h gmshModelGeoAddPoint"
  cgmshModelGeoAddPoint :: CDouble
                        -> CDouble
                        -> CDouble
                        -> CDouble
                        -> CInt
                        -> Ptr CInt
                        -> IO()

gmshModelGeoSynchronize :: IO(Int)
gmshModelGeoSynchronize = do
  alloca $ \errptr -> do
    cgmshModelGeoSynchronize errptr
    errcode <- peek errptr
    return $ fromIntegral errcode

foreign import ccall unsafe "gmshc.h gmshModelGeoSynchronize"
  cgmshModelGeoSynchronize :: Ptr CInt -> IO()

gmshFltkRun :: IO(Int)
gmshFltkRun = do
  alloca $ \errptr -> do
    cgmshFltkRun errptr
    errcode <- peek errptr
    return $ fromIntegral errcode

foreign import ccall unsafe "gmshc.h gmshFltkRun"
  cgmshFltkRun :: Ptr CInt -> IO()

flatTo2Tuple :: V.Vector a -> V.Vector (a,a)
flatTo2Tuple xs = V.zip xs (V.tail xs)

peekVector len arr = V.fromList <$> peekArray len arr

-- Ptr (Ptr CInt) is a serialized list of integers, i.e.
-- **int is a pointer to an array, not an array of arrays... D'OH!
peekDimTags :: Int -> Ptr (Ptr CInt) -> IO(V.Vector (Int, Int))
peekDimTags ndimTags arr  = do
  arr' <- peek arr
  dimTags <- peekVector ndimTags arr'
  return $ flatTo2Tuple $ fmap fromIntegral dimTags

gmshModelGetEntities :: Int -> IO(V.Vector (Int, Int), Int)
gmshModelGetEntities dim = do
  let dim' = fromIntegral dim
  alloca $ \errptr -> do
    alloca $ \ndimTags -> do
      alloca $ \dimTags -> do
        cgmshModelGetEntities dimTags ndimTags dim' errptr
        errcode <- (fromIntegral <$> (peek errptr))
        ndimTags' <- peek ndimTags
        dimTags' <- peekDimTags (fromIntegral ndimTags') dimTags
        print dimTags'
        return (dimTags', errcode)

foreign import ccall unsafe "gmshc.h gmshModelGetEntities"
 cgmshModelGetEntities :: Ptr (Ptr CInt) -> Ptr CInt -> CInt -> Ptr CInt -> IO()
