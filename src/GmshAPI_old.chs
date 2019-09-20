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

module GmshAPI where

import Control.Monad (liftM)
import Foreign.C -- get the C types
import Foreign.Ptr (Ptr,nullPtr)
import Foreign.C.String (withCString)
import Foreign.Marshal (alloca)
import Foreign.Marshal.Utils (withMany)
import Foreign.Marshal.Array (withArray)
import Foreign.Storable (peek)
import Data.Maybe (fromMaybe)


#include "gmshc.h"

{#context lib="gmsh" #}

toInt :: Ptr CInt -> IO Int
toInt = liftM fromIntegral . peek

-- marshaller for argv type "char ** argv"
argv :: [String] -> (Ptr CString -> IO a) -> IO a
argv ss f = withMany withCString ss f'
   where
      -- my brain hurts but at least I can hear colors now
      f' x = withArray x f

{-
GMSH_API void gmshInitialize(int argc, char ** argv,
                             const int readConfigFiles,
                             int * ierr);
-}
{#fun gmshInitialize as gmshInitialize
    {  `Int'
    , argv* `[String]' void-
    ,  `Int'
    , alloca- `Int' toInt*
    } -> `()' #}

{-GMSH_API void gmshFinalize(int * ierr);-}
{#fun gmshFinalize as gmshFinalize
   { alloca- `Int' toInt*
   } -> `()' #}

{-GMSH_API void gmshFltkRun(int * ierr);-}
{#fun gmshFltkRun as gmshFltkRun
   { alloca- `Int' toInt*
   } -> `()' #}

{-GMSH_API int gmshModelGeoAddPoint(const double x,
                                  const double y,
                                  const double z,
                                  const double meshSize,
                                  const int tag,
                                  int * ierr);-}
{#fun gmshModelOccAddPoint as gmshModelGeoAddPoint
   { `Double'
   , `Double'
   , `Double'
   , `Double'
   , `Int'
   , alloca- `Int' toInt*
   } -> `()' #}

{-GMSH_API void gmshModelGeoSynchronize(int * ierr);-}
{#fun gmshModelOccSynchronize as gmshModelGeoSynchronize
   { alloca- `Int' toInt*
   } -> `()' #}
