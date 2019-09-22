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
    def __init__(self, name, value, haskell_type, out):
        self.name = name
        self.value = value
        self.out = out
        self.haskell_type = haskell_type

# input types

def ibool(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "Bool", False)
    return a

def iint(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "Int", False)
    return a

def isize(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "Int", False)
    return a

def idouble(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "Double", False)
    return a

def istring(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "String", False)
    return a

def ivoidstar(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, None, False)
    return a

def ivectorint(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[Int]", False)
    return a

def ivectorsize(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[Int]", False)
    return a

def ivectordouble(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[Double]", False)
    return a

def ivectorstring(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "String", False)
    return a

def ivectorpair(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[(Int, Int)]", False)
    return a

def ivectorvectorint(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[[Int]]", False)
    return a

def ivectorvectorsize(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[[Int]]", False)
    return a

def ivectorvectordouble(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[[Double]]", False)
    return a

# output types

class oint(arg):
    rhaskell_type = "?"
    def __init__(self, name, value=None, python_value=None, julia_value=None):
        arg.__init__(self, name, value, "Int", True)

class osize(arg):
    def __init__(self, name, value=None, python_value=None, julia_value=None):
        arg.__init__(self, name, value, "Int", True)

class odouble(arg):
    def __init__(self, name, value=None, python_value=None, julia_value=None):
        arg.__init__(self, name, value, "Int", True)


def ostring(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "String", True)
    return a

def ovectorint(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[Int]", True)

    return a

def ovectorsize(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[Int]", True)
    return a

def ovectordouble(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[Double]", True)
    return a

def ovectorstring(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[String]", True)
    return a

def ovectorpair(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[(Int,int)]", True)
    return a

def ovectorvectorint(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[Int]", True)
    return a

def ovectorvectorsize(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[Int]", True)
    return a

def ovectorvectordouble(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[[Double]]", True)
    return a

def ovectorvectorpair(name, value=None, python_value=None, julia_value=None):
    a = arg(name, value, "[[Int]]", True)
    return a

def argcargv():
    a = arg("argv", None, "argv* `[String]' void-", False)

    return a

def camelcasify(str):
    """ raise the first letter to uppercase """
    return str[0].upper() + str[1:]

class Module:

    def __init__(self, name, doc):
        self.name = name
        self.doc = doc
        self.fs = []
        self.submodules = []

    def add(self, name, doc, rtype, *args):
        self.fs.append((rtype, name, args, doc, []))

    def add_special(self, name, doc, special, rtype, *args):
        self.fs.append((rtype, name, args, doc, special))

    def add_module(self, name, doc):
        module = Module(name, doc)
        self.submodules.append(module)
        return module


    """{#fun gmshInitialize as gmshInitialize
        {  `Int'
        , argv* `[String]' void-
        ,  `Int'
        , alloca- `Int' toInt*
        } -> `()' #}"""

    def write_module(self, fhandle, **kwargs):
        fwdkwargs = kwargs.copy()
        kwprefix = kwargs.get('prefix', None)

        if kwprefix is None:
            prefix = self.name
            fwdkwargs['prefix'] = prefix
        else:
            prefix = kwprefix+camelcasify(self.name)
            fwdkwargs['prefix'] = prefix

        # First write c2hs definitions of functions in this module
        # and then advance to submodules
        for rtype, name, args, doc, _ in self.fs:
            fun = prefix + camelcasify(name)
            # First put the docstring and the function name
            fundef = ["{{-{}-}}\n".format(doc),
                      "{{#fun {} as {}\n".format(fun, fun)]

            # Every function has the error code, but if we have other
            # arguments, list them here. In order for the Haskell style
            # pretty printing to work, we need to handle the first argument
            # separately
            if len(args) > 0:
                fst_a = args[0]
                fundef.append("   {{ `{}' -- {}\n".format(fst_a.haskell_type,
                                                          fst_a.name))
                # shove the rest of the arguments in
                for a in args[1:]:
                    fundef.append("   , `{}' -- {}\n".format(a.haskell_type,
                                                             a.name))

                fundef.append("   , alloca- `Int' toInt* -- error code\n")

            else:
                fundef.append("   { alloca- `Int' toInt* -- error code\n")

            if rtype is None:
                fundef.append("} -> `()' #}\n")
            else:
                fundef.append("}} -> `{}' #}}\n".format(rtype))
            fhandle.writelines(fundef)
            fhandle.write("\n")

        for m in self.submodules:
            m.write_module(fhandle, **fwdkwargs)
            #fhandle.write(f"{rtype} - {name} - {doc}\n")


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

-- marshaller for error code
toInt :: Ptr CInt -> IO Int
toInt = liftM fromIntegral . peek

-- marshaller for argv type "char ** argv"
argv :: [String] -> (Ptr CString -> IO a) -> IO a
argv ss f = withMany withCString ss f'
   where
      f' x = withArray x f

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

        with open("GmshAPI.chs", 'w') as f:
            f.write(haskell_header)
            for m in self.modules:
                m.write_module(f)
