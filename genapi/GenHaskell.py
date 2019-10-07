# This is a heavily adopted version of the Gmsh API generation file
# for the purpose of generating Haskell c2hs definitions from the
# Gmsh API definitions file:
# https://gitlab.onelab.info/gmsh/gmsh/blob/master/api/gen.py

# Original file can be found from:
# https://gitlab.onelab.info/gmsh/gmsh/blob/master/api/GenApi.py

# Credit to
# Gmsh - Copyright (C) 1997-2019 C. Geuzaine, J.-F. Remacle
# Contributor(s):
#   Jonathan Lambrechts

#
# Modifications for Haskell by Antero Marjamäki (first.last@tuni.fi)
#



class arg:
    output = False
    def __init__(self, name, value=None,  python_value=None, julia_value=None):
        self.name = name
        self.value = value

# input types
class ibool(arg):
    htype = "Bool"
    ctype = "CBool"

class iint(arg):
    htype = "Int"
    ctype = "CInt"

class isize(arg):
    htype = "Int"
    ctype = "CInt"

class idouble(arg):
    htype = "Double"
    ctype = "CDouble"

class istring(arg):
    htype = "String"
    ctype = "CString"

class ivoidstar(arg):
    htype = "?"
    ctype = "?"


class ivectorint(arg):
    htype = "[Int]"
    ctype = "Ptr CInt"

class ivectorsize(arg):
    htype = "[Int]"
    ctype = "Ptr CInt"

class ivectordouble(arg):
    htype = "[Double]"
    ctype = "Ptr Double"

class ivectorstring(arg):
    htype = "[String]"
    ctype = "Ptr CString"

class ivectorpair(arg):
    htype = "[(Int, Int)]"
    ctype = "Ptr (Ptr Int)"

# seems like not in use
# class ivectorvectorint(arg):
#     htype = ""
#     ctype = ""

class ivectorvectorsize(arg):
    htype = "[[Int]]"
    ctype = "Ptr (Ptr Int)"

class ivectorvectordouble(arg):
    htype = "[[Double]]"
    ctype = "Ptr (Ptr Double)"

# output types

class oint(iint):
    output = True
    htype = "Int"
    ctype = "Ptr CInt"

class osize(isize):
    output = True
    htype = "Int"
    ctype = "Ptr CInt"

class odouble(idouble):
    output = True
    htype = "Double"
    ctype = "Ptr CDouble"

class ostring(istring):
    output = True
    htype = "String"
    ctype = "Ptr CString"

class ovectorint(ivectorint):
    output = True
    htype = "[Int]"
    ctype = "Ptr CInt"

class ovectorsize(ivectorsize):
    output = True
    htype = "[Int]"
    ctype = "Ptr CInt"

class ovectordouble(ivectordouble):
    output = True
    htype = "[Double]"
    ctype = "Ptr CDouble"

class ovectorstring(ivectorstring):
    output = True
    htype = "[String]"
    ctype = "Ptr CString"

class ovectorpair(ivectorpair):
    output = True
    htype = "Int"
    ctype = "Ptr CInt"

# Not used
# class ovectorvectorint(ivectorvectorint):
#     output = True

class ovectorvectorsize(ivectorvectorsize):
    output = True
    htype = "[[Int]]"
    ctype = "Ptr (Ptr CInt)"

class ovectorvectordouble(ivectorvectordouble):
    output = True
    htype = "[[Double]]"
    ctype = "Ptr (Ptr CDouble)"

class ovectorvectorpair(arg):
    output = True
    htype = "[[(Int, Int)]]"
    ctype = "Ptr (Ptr (Ptr Int))"

class argcargv():
    output = False
    def __init__(self, *args):
        self.name = "argv"
        self.htype = "argv* `[String]' void-"
        self.ctype = "Ptr CString"

def camelcasify(str):
    """ raise the first letter to uppercase """
    return str[0].upper() + str[1:]

class Function:
    def __init__(self, rtype, name, args, doc, special=[]):
        self.return_type = rtype
        self.name = name
        self.args = args
        self.doc = doc
        self.special = special

    def to_string(self, prefix):
        return "\n".join([self.str_type_signature(prefix),
                          self.str_foreignexp(prefix)])

    def str_type_signature(self, prefix):
        """ Generate the type signature for the haskell function
        Input variables need to be present, output variables are wrapped
        in the IO action.
        Example:

        gmshModelGetEntities :: Int -> IO([(Int, Int)]) """

        """ ATTENTION: the vector sizes are not present in the
        api_gen.py. Probably need to refactor this to
        the argument class (argument class needs to create
        a string for itself, only it knows when extra sizes are present
        in the c call.) """
        fname = prefix  + camelcasify(self.name)
        inputs = [a for a in self.args if a.output]
        outputs = [a for a in self.args if not a.output]

        itypesign = " -> ".join([a.htype for a in inputs])
        if(len(itypesign) > 0):
            itypesign += " -> "

        otypesign = ", ".join([a.htype for a in outputs])

        return "".join([fname, " :: ",  itypesign, "IO(", otypesign, ")"])


    def str_foreignexp(self, prefix):
        """
        Generates the foreign import statement for the function, as a string.

        Example:

        foreign import ccall unsafe "gmshc.h gmshModelGetEntities"
         cgmshModelGetEntities
            :: Ptr (Ptr CInt)
            -> Ptr CInt
            -> CInt
            -> Ptr CInt
            -> IO()

        """
        """ ATTENTION: the vector sizes are not present in the
        api_gen.py. Probably need to refactor this to
        the argument class (argument class needs to create
        a string for itself, only it knows when extra sizes are present
        in the c call.) """

        fname = prefix + camelcasify(self.name)
        lines = ["foreign import ccall unsafe \"gmshc.h {}\"".format(fname)]
        lines.append("   {}".format("c"+fname))

        # print arguments (c types), always at least errorcode
        # which is not present in the api definition
        args = list(self.args) + [oint("errcode")]
        lines.append("      :: {}".format(args[0].ctype))
        for a in args[1:]:
            lines.append("      -> {}".format(a.ctype))

        if self.return_type is None:
            lines.append("      -> IO()")
        else:
            lines.append("      -> IO({})".format(self.return_type.ctype))

        return "\n".join(lines)

class Module:

    def __init__(self, name, doc):
        self.name = name
        self.doc = doc
        self.fs = []
        self.submodules = []

    def add(self, name, doc, rtype, *args):
        self.fs.append(Function(rtype, name, args, doc, []))

    def add_special(self, name, doc, special, rtype, *args):
        self.fs.append(Function(rtype, name, args, doc, special))

    def add_module(self, name, doc):
        module = Module(name, doc)
        self.submodules.append(module)
        return module

    def write_module(self, fhandle, **kwargs):
        fwd_kwargs = kwargs.copy()
        kwprefix = kwargs.get('prefix', None)

        if kwprefix is None:
            prefix = self.name
            fwd_kwargs['prefix'] = prefix
        else:
            prefix = kwprefix+camelcasify(self.name)
            fwd_kwargs['prefix'] = prefix

        #fhandle.write("Module {}: \n".format(prefix))
        # take only two functions from each module at first
        for f in self.fs[:2]:
            fhandle.write(f.to_string(prefix))
            fhandle.write("\n")

        for m in self.submodules:
            m.write_module(fhandle, **fwd_kwargs)

haskell_header = """
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
import Foreign.Storable (peek, Storable)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

--import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as V


--include "gmshc.h"

toInt :: Ptr CInt -> IO Int
toInt = liftM fromIntegral . peek

-- marshaller for argv type "char ** argv"
argv :: [String] -> (Ptr CString -> IO a) -> IO a
argv ss f = withMany withCString ss f'
   where
      f' x = withArray x f

flatTo2Tuple :: [a] -> [(a,a)]
flatTo2Tuple (x:y:[]) = [(x,y)]
flatTo2Tuple (x:y:xs) = (x,y) : flatTo2Tuple xs

-- Ptr (Ptr CInt) is a serialized list of integers, i.e.
-- **int is a pointer to an array, not an array of arrays... D'OH!
peekDimTags :: Int -> Ptr (Ptr CInt) -> IO([(Int, Int)])
peekDimTags ndimTags arr  = do
  arr' <- peek arr
  dimTags <- peekArray ndimTags arr'
  return $ flatTo2Tuple $ map fromIntegral dimTags

"""

class API:

    def __init__(self, version_major, version_minor, namespace="gmsh", code="Gmsh",
                 copyright="Gmsh - Copyright (C) 1997-2019 C. Geuzaine, J.-F. Remacle",
                 issues="https://gitlab.onelab.info/gmsh/gmsh/issues."):
        self.version_major = version_major
        self.version_minor = version_minor
        global ns
        ns = namespace
        self.code = code
        self.copyright = copyright
        self.issues = issues
        self.modules = []

    def add_module(self, name, doc):
        module = Module(name, doc)
        self.modules.append(module)
        return module

    def write_julia(self):
        pass

    def write_texi(self):
        pass

    def write_cpp(self):
        pass

    def write_c(self):
        pass

    def write_python(self):
        """ Just kidding! Write Haskell instead! """

        with open("GmshAPI.hs", 'w') as f:
            f.write(haskell_header)
            for m in self.modules:
                m.write_module(f)
