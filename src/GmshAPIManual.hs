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
import Foreign.Marshal.Array (withArray, peekArray, advancePtr, withArrayLen)
import Foreign.Storable (peek, Storable)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

--import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as V


--include "gmshc.h"

toInt :: Ptr CInt -> IO Int
toInt = liftM fromIntegral . peek

-- marshaller for argv type "char ** argv"
withArgv :: [String] -> (Ptr CString -> IO a) -> IO a
withArgv ss f = withMany withCString ss f'
   where
      f' x = withArray x f

peekInt :: Ptr CInt -> IO(Int)
peekInt = liftM fromIntegral . peek


flatToPairs :: [a] -> [(a,a)]
flatToPairs [] = []
flatToPairs (x:y:[]) = [(x,y)]
flatToPairs (x:y:xs) = (x,y) : flatToPairs xs

pairsToFlat :: [(a,a)] -> [a]
pairsToFlat lst = reverse $ foldl (\acc (a,b) -> b:a:acc) [] lst


-- Ptr (Ptr CInt) is a serialized list of integers, i.e.
-- **int is a pointer to an array, not an array of arrays... D'OH!
peekArrayPairs :: Ptr CInt -> Ptr (Ptr CInt) -> IO([(Int, Int)])
peekArrayPairs nptr arrptr  = do
  npairs <- peekInt nptr
  arr <- peek arrptr
  flatpairs <- peekArray npairs arr
  return $ flatToPairs $ map fromIntegral flatpairs

--foldfun :: ([[(CInt, CInt)]], Ptr (Ptr CInt))
-- -> Int -> IO([[(CInt, CInt)]], Ptr (Ptr CInt))

peekArrayArrayPairs
  :: Ptr CInt
  -> Ptr (Ptr CInt)
  -> Ptr (Ptr (Ptr CInt))
  -> IO([[(Int, Int)]])
peekArrayArrayPairs lengthLengthsPtr lengthsPtr arrPtr  =
  do
    nlengths <- peekInt lengthLengthsPtr
    lengthsArr <- peek lengthsPtr
    lengthsList <- peekArray nlengths lengthsArr
    arrptr <- peek arrPtr
    -- okay, so. fold over lengthsList.
    -- For each element dereference the pointer
    -- then peek n elements from the array, then advance the outer pointer
    -- accumulate the list of peeked lists, and the advanced pointer
    (pairs,_) <- foldl foldfun (return ([], arrptr)) $ map fromIntegral lengthsList
    return pairs

  where
    -- foldfun takes the previous IO action and runs it,
    -- then proceeds to peek and advance ptrs and wraps the results
    -- in a tuple
    foldfun action n = do
        (acc, ptr) <- action
        aptr <- peek ptr
        lst <- peekArray n aptr
        let pairss = flatToPairs $ map fromIntegral lst
        let newptr = advancePtr ptr 1
        return ((pairss:acc), newptr)



checkErrorCodeAndThrow :: String -> Ptr CInt -> IO()
checkErrorCodeAndThrow funname errptr = do
  errcode <- peekInt errptr
  if errcode == 0
      then return ()
      else error $ funname ++ " returned nonzero error code: " ++ show errcode



gmshInitialize :: Int -> [String] -> Int -> IO()
gmshInitialize argc ss readConfigFiles = do
  let argc' = fromIntegral argc
  let readConfigFiles' = fromIntegral readConfigFiles
  withArgv ss $ \argv' -> do
    alloca $ \errptr -> do
      cgmshInitialize argc' argv' readConfigFiles' errptr
      checkErrorCodeAndThrow "gmshInitialize" errptr
      return ()

foreign import ccall unsafe "gmshc.h gmshInitialize"
  cgmshInitialize :: CInt
                  -> (Ptr CString)
                  -> CInt
                  -> Ptr CInt
                  -> IO()

gmshModelOccAddPoint :: Double -> Double -> Double -> Double -> Int -> IO()
gmshModelOccAddPoint x y z c tag = do
  let x' = realToFrac x
  let y' = realToFrac y
  let z' = realToFrac z
  let c' = realToFrac c
  let tag' = fromIntegral tag
  alloca $ \errptr -> do
    cgmshModelOccAddPoint x' y' z' c' tag' errptr
    checkErrorCodeAndThrow "gmshModelOccAddPoint" errptr
    return ()

foreign import ccall unsafe "gmshc.h gmshModelOccAddPoint"
  cgmshModelOccAddPoint
    :: CDouble
    -> CDouble
    -> CDouble
    -> CDouble
    -> CInt
    -> Ptr CInt
    -> IO()

gmshModelOccSynchronize :: IO()
gmshModelOccSynchronize = do
  alloca $ \errptr -> do
    cgmshModelOccSynchronize errptr
    checkErrorCodeAndThrow "gmshModelOccSynchronize" errptr
    return ()

foreign import ccall unsafe "gmshc.h gmshModelOccSynchronize"
  cgmshModelOccSynchronize :: Ptr CInt -> IO()

gmshFltkRun :: IO()
gmshFltkRun = do
  alloca $ \errptr -> do
    cgmshFltkRun errptr
    checkErrorCodeAndThrow "gmshFltkRun" errptr
    return ()

foreign import ccall unsafe "gmshc.h gmshFltkRun"
  cgmshFltkRun :: Ptr CInt -> IO()



{-
GMSH_API void gmshModelOccFuse(int * objectDimTags, size_t objectDimTags_n,
                               int * toolDimTags, size_t toolDimTags_n,
                               int ** outDimTags, size_t * outDimTags_n,
                               int *** outDimTagsMap, size_t ** outDimTagsMap_n, size_t *outDimTagsMap_nn,
                               const int tag,
                               const int removeObject,
                               const int removeTool,
                               int * ierr);
-}


gmshModelOccFuse
  :: [(Int, Int)]
  -> [(Int, Int)]
  -> Int
  -> Int
  -> Int
  -> IO([(Int, Int)], [[(Int, Int)]])
gmshModelOccFuse objectDimTags toolDimTags tag removeObject removeTool = do
  let tag' = fromIntegral tag
  let removeObject' = fromIntegral removeObject
  let removeTool' = fromIntegral removeTool
  let objectDimTags' = map fromIntegral $ pairsToFlat objectDimTags
  let toolDimTags' = map fromIntegral $ pairsToFlat toolDimTags
  alloca $ \noutDimTagsPtr -> do
    alloca $ \outDimTagsPtr -> do
      alloca $ \nnoutDimTagsMapPtr -> do
        alloca $ \noutDimTagsMapPtr -> do
          alloca $ \outDimTagsMapPtr-> do
            alloca $ \errptr -> do
                withArrayLen objectDimTags' $ \nobjectDimTags objectDimTagsPtr -> do
                    withArrayLen toolDimTags' $ \ntoolDimTags toolDimTagsPtr -> do
                      let nobjectDimTags' = fromIntegral nobjectDimTags
                      let ntoolDimTags' = fromIntegral ntoolDimTags
                      cgmshModelOccFuse objectDimTagsPtr nobjectDimTags' toolDimTagsPtr ntoolDimTags' outDimTagsPtr noutDimTagsPtr outDimTagsMapPtr nnoutDimTagsMapPtr noutDimTagsMapPtr tag' removeObject' removeTool' errptr
                      checkErrorCodeAndThrow "gmshModelOccFuse" errptr
                      outDimTags <- peekArrayPairs noutDimTagsPtr outDimTagsPtr
                      outDimTagsMap <- peekArrayArrayPairs noutDimTagsMapPtr nnoutDimTagsMapPtr outDimTagsMapPtr

                      return (outDimTags, outDimTagsMap)

foreign import ccall unsafe "gmshc.h gmshModelOccFuse"
  cgmshModelOccFuse
    :: Ptr CInt -> CInt
    -> Ptr CInt -> CInt
    -> Ptr (Ptr CInt) -> Ptr CInt
    -> Ptr (Ptr (Ptr CInt)) -> Ptr (Ptr CInt) -> Ptr CInt
    -> CInt
    -> CInt
    -> CInt
    -> Ptr CInt
    -> IO()

gmshModelGetEntities :: Int -> IO([(Int, Int)])
gmshModelGetEntities dim = do
  let dim' = fromIntegral dim
  alloca $ \dimTags -> do
    alloca $ \ndimTags -> do
      alloca $ \errptr -> do
        cgmshModelGetEntities dimTags ndimTags dim' errptr
        checkErrorCodeAndThrow "gmshModelGetEntities" errptr
        dimTags' <- peekArrayPairs ndimTags dimTags
        return dimTags'

foreign import ccall unsafe "gmshc.h gmshModelGetEntities"
  cgmshModelGetEntities :: Ptr (Ptr CInt) -> Ptr CInt -> CInt -> Ptr CInt -> IO()


{-  GMSH_API int gmshModelOccAddDisk(const double xc,
                                   const double yc,
                                   const double zc,
                                   const double rx,
                                   const double ry,
                                   const int tag,
                                   int * ierr);-}

gmshModelOccAddDisk
  :: Double -> Double -> Double
  -> Double -> Double
  -> Int -> IO()
gmshModelOccAddDisk xc yc zc rx ry tag = do
  let xc' = realToFrac xc
  let yc' = realToFrac yc
  let zc' = realToFrac zc
  let rx' = realToFrac rx
  let ry' = realToFrac ry
  let tag' = fromIntegral tag
  alloca $ \errptr -> do
    cgmshModelOccAddDisk xc' yc' zc' rx' ry' tag' errptr
    checkErrorCodeAndThrow "gmshOccAddDisk" errptr
    return ()

foreign import ccall unsafe "gmshc.h gmshModelOccAddDisk"
 cgmshModelOccAddDisk
  :: CDouble -> CDouble -> CDouble
  -> CDouble -> CDouble
  -> CInt -> Ptr CInt -> IO()
