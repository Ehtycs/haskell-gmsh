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
    def __init__(self, name=None, value=None,  python_value=None, julia_value=None):
        self.name = name
        self.value = value

    def foreignexp(self):
        return [self.ctype]

    def type_signature(self):
        return [self.htype]

class oarg(arg):
    output = True
    def foreignexp(self):
        # these things are either the return values of the C function
        # or ordinary "reference" variables
        return ["Ptr {}".format(self.ctype)]

    def type_signature(self):
        return [self.htype]

    def type_in_return(self):
        return [self.ctype]



# these things are not given as return values from the C api,
# because that's impossible
class input_array(arg):
    def foreignexp(self):
        out = ["Ptr {}".format(self.ctype), "CInt"]
        return out

class input_arrayarray(arg):
    def foreignexp(self):
        out = ["Ptr (Ptr {})".format(self.ctype), "Ptr CInt", "CInt"]
        return out

class output_array(oarg):
    def foreignexp(self):
        return ["Ptr ( Ptr {})".format(self.ctype),
                "Ptr CInt"]

class output_arrayarray(oarg):
    def foreignexp(self):
        return ["Ptr (Ptr (Ptr {}))".format(self.ctype),
                "Ptr (Ptr CInt)",
                "Ptr CInt"]

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


class ivectorint(input_array):
    htype = "[Int]"
    ctype = "CInt"


class ivectorsize(input_array):
    htype = "[Int]"
    ctype = "CInt"

class ivectordouble(input_array):
    htype = "[Double]"
    ctype = "Double"


class ivectorstring(input_array):
    htype = "[String]"
    ctype = "CString"


class ivectorpair(input_array):
    htype = "[(Int, Int)]"
    ctype = "CInt"


# seems like not in use
# class ivectorvectorint(arg):
#     htype = ""
#     ctype = ""

class ivectorvectorsize(input_arrayarray):
    htype = "[[Int]]"
    ctype = "CInt"

class ivectorvectordouble(input_arrayarray):
    htype = "[[Double]]"
    ctype = "CDouble"

# output types

class oint(oarg):
    htype = "Int"
    ctype = "CInt"

class osize(oarg):
    htype = "Int"
    ctype = "CInt"

class odouble(oarg):
    htype = "Double"
    ctype = "CDouble"

class ostring(oarg):
    htype = "String"
    ctype = "CString"

class ovectorint(output_array):
    htype = "[Int]"
    ctype = "CInt"

class ovectorsize(output_array):
    htype = "[Int]"
    ctype = "CInt"

class ovectordouble(output_array):
    htype = "[Double]"
    ctype = "CDouble"


class ovectorstring(output_array):
    htype = "[String]"
    ctype = "CString"

class ovectorpair(output_array):
    htype = "Int"
    ctype = "CInt"


# Not used
# class ovectorvectorint(ivectorvectorint):
#     output = True

class ovectorvectorsize(output_arrayarray):
    output = True
    htype = "[[Int]]"
    ctype = "CInt"

class ovectorvectordouble(output_arrayarray):
    output = True
    htype = "[[Double]]"
    ctype = "CDouble"

class ovectorvectorpair(output_arrayarray):
    output = True
    htype = "[[(Int, Int)]]"
    ctype = "CInt"


class argcargv(arg):
    output = False
    def __init__(self, *args):
        self.name = "argv"
        self.htype = "argv* `[String]' void-"
        self.ctype = "Ptr CString"

def camelcasify(str):
    """ raise the first letter to uppercase """
    return str[0].upper() + str[1:]

def flatten2(lst):
    return [x for xs in lst for x in xs]

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

        If function has the return_type specified, it will always be the last
        one in IO(...)

        Example:

        gmshModelGetEntities :: Int -> IO([(Int, Int)]) """

        """ ATTENTION: the vector sizes are not present in the
        api_gen.py. Probably need to refactor this to
        the argument class (argument class needs to create
        a string for itself, only it knows when extra sizes are present
        in the c call.) """
        fname = prefix  + camelcasify(self.name)
        inputs = [a for a in self.args if not a.output]
        outputs = [a for a in self.args if a.output]

        # append the return_type, if any
        if self.return_type is not None:
            outputs.append(self.return_type)

        itypes = flatten2([a.type_signature() for a in inputs])
        itypesign = " -> ".join(itypes)

        if(len(itypesign) > 0):
            itypesign += " -> "

        otypes = flatten2([a.type_signature() for a in outputs])
        otypesign = ", ".join(otypes)

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
        types = flatten2([a.foreignexp() for a in args])

        lines.append("      :: {}".format(types[0]))
        for t in types[1:]:
            lines.append("      -> {}".format(t))

        if self.return_type is None:
            lines.append("      -> IO()")
        else:
            t = self.return_type.type_in_return()[0]
            lines.append("      -> IO({})".format(t))

        return "\n".join(lines)

class Module:

    def __init__(self, name, doc):
        self.name = name
        self.doc = doc
        self.fs = []
        self.submodules = []

    def add(self, name, doc, rtype, *args):
        if rtype is not None:
            rtype = rtype()
        self.fs.append(Function(rtype, name, args, doc, []))

    def add_special(self, name, doc, special, rtype, *args):
        if rtype is not None:
            rtype = rtype()
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
