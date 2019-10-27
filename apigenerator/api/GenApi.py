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


def haskellify_default_value(value, pyvalue):
    """ Take the C-default value and pyvalue and convert them to
    a haskell value. Haskell is a bit silly with negative numbers,
    they need to be wrapped in parentheses.
    Also . is reserved for function composition, so it must be 1.0 not 1."""
    if(pyvalue is None):
        if(value is None):
            return None
        elif(value == ""):
            return "\"\""
        elif(value[-1] == "."):
            return f"{value}0"
        else:
            return value
    else:
        return pyvalue



class arg:
    """ Basic datatype of an argument. Every datatype inherits this constructor
    and also default behaviour from here.

    The methods all return lists of strings, which are then combined on the
    functions which really construct the function definitions etc.
    """
    output = False  # whether this is an output argument or not
    indent = 0      # how many levels of indentation is needed on
                    # function body

    def __init__(self, name=None, value=None,  python_value=None, julia_value=None):

        # "data" is a reserved keyword in haskell so mangle any
        # instances of it
        if name == "data":
            name = "daatta"

        self.name = name
        # python value is easier to translate to Haskell,
        # use it if available, otherwise use the c default value
        self.value = haskellify_default_value(value, python_value)


    def input_name(self):
        """ Optional arguments get a trailing "Maybe" """
        if(self.value is None):
            return self.name
        else:
            return self.name+"Maybe"

    def foreignexp(self):
        """ This "renders" the argument to the "foreign import ccall" line """
        return [self.ctype]

    def type_signature(self):
        """ This "renders" the argument to the type signature line """
        if self.value == None:
            return [self.htype]
        else:
            # If argument has a default value, it's optional. Hence
            # wrap it in a Maybe
            return [f"Maybe {self.htype}"]

    def ccall_inputs(self):
        """ This "renders" the argument to the c call line """
        return f"{self.name}'"

    def marshall_in(self):
        """ This is the way it is because of the default arguments given to
        some of the functions. They are represented by a "Maybe" type.
        For those, we need to prepend an unwrapping operation. _marshall_in
        method is the one the subclasses override IF THEY NEED this behaviour.
        Otherwise they can just override this marshall_in method. """
        n = self.name
        dv = self.value
        lines = []
        if(dv is not None):
            lines.append(f"let {n} = fromMaybe ({dv}) {n}Maybe")
        for ln in self._marshall_in():
            lines.append(ln)
        return lines

class oarg(arg):
    """ Basic datatype of an output argument (basically c pointer of some sort)
    in the c API call.

    In Haskell, these get wrapped to Ptr CX types where X is usually
    Int, Double, etc... but they must be converted to real Haskell values
    before returning them from the API functions."""

    output = True

    def __init__(self, name, value=None, *args):
        # I'm assuming that output arguments don't have a default value
        # because it's not reasonable.
        assert value == None, "Output argument shouldn't have default value"
        super().__init__(name, value, *args)


    def foreignexp(self):
        return [f"Ptr {self.ctype}"]

    def type_signature(self):
        return [self.htype]

    def type_in_return(self):
        return [self.ctype]

    def return_name(self):
        return f"{self.name}'''"


class input_array(arg):
    """ Base class for input arrays. These things have a lenght and a pointer
    in the C API calls .
    """
    indent = 1

    def foreignexp(self):
        out = [f"Ptr {self.ctype}", "CInt"]
        return out

    def ccall_inputs(self):
        n = self.name
        return f"{n}' {n}_n'"

class input_arrayarray(arg):
    """ Base class for 2-dimensional input arrays. These things have length
    (dim 1), an array of lengths (in dim 2) and a double pointer to the actual
    data.


    """

    indent = 2

    def foreignexp(self):
        out = [f"Ptr (Ptr {self.ctype})", "Ptr CInt", "CInt"]
        return out

    def ccall_inputs(self):
        n = self.name
        return f"{n}' {n}_n' {n}_nn'"

class output_array(oarg):
    """ Base class for output arrays. These have a pointer to length, and a
    double pointer in the C API
    """
    indent = 1


    def foreignexp(self):
        return [f"Ptr ( Ptr {self.ctype})",
                "Ptr CInt"]

    def ccall_inputs(self):
        n = self.name
        return f"{n}' {n}_n'"

class output_arrayarray(oarg):
    """ Base class for multidimensional output arrays. These have basically
    just one more level of pointers to same things as input_arrayarray """

    indent = 2

    def foreignexp(self):
        return [f"Ptr (Ptr (Ptr {self.ctype}))",
                "Ptr (Ptr CInt)",
                "Ptr CInt"]

    def ccall_inputs(self):
        n = self.name
        return f"{n}' {n}_n' {n}_nn'"

# input types
class ibool(arg):
    htype = "Bool"
    ctype = "CBool"

    def _marshall_in(self):
        n = self.name
        return [f"let {n}' = fromBool {n}"]

class iint(arg):
    htype = "Int"
    ctype = "CInt"

    def _marshall_in(self):
        n = self.name
        return [f"let {n}' = fromIntegral {n}"]

class isize(arg):
    htype = "Int"
    ctype = "CInt"

    def _marshall_in(self):
        n = self.name
        return [f"let {n}' = fromIntegral {n}"]

class idouble(arg):
    htype = "Double"
    ctype = "CDouble"

    def _marshall_in(self):
        n = self.name
        return [f"let {n}' = realToFrac {n}"]

class istring(arg):
    htype = "String"
    ctype = "CString"
    indent = 1

    def _marshall_in(self):
        n = self.name
        return [f"withCString {n} $ \\{n}' -> do"]

class ivoidstar(arg):
    """
    This can't be used but needs to be defined in order to run the api_gen
    """
    htype = "?"
    ctype = "?"

    def marshall_in(self):
        assert False , "Datatype void* is not applicable in Haskell"
        return None

class ivectorint(input_array):
    htype = "[Int]"
    ctype = "CInt"

    def _marshall_in(self):
        n = self.name
        return [f"withArrayIntLen {n} $ \\{n}_n' {n}' -> do"]

class ivectorsize(input_array):
    htype = "[Int]"
    ctype = "CInt"

    def _marshall_in(self):
        n = self.name
        return [f"withArrayIntLen {n} $ \\{n}_n' {n}' -> do"]

class ivectordouble(input_array):
    htype = "[Double]"
    ctype = "CDouble"

    def _marshall_in(self):
        n = self.name
        return [f"withArrayDoubleLen {n} $ \\{n}_n' {n}' -> do"]

class ivectorstring(input_array):
    htype = "[String]"
    ctype = "CString"

    def _marshall_in(self):
        n = self.name
        return [f"withArrayStringLen {n} $ \\{n}_n' {n}' -> do"]

class ivectorpair(input_array):
    htype = "[(Int, Int)]"
    ctype = "CInt"

    def _marshall_in(self):
        n = self.name
        return [f"withArrayPairLen {n} $ \\{n}_n' {n}' -> do"]


# seems like not in use
# class ivectorvectorint(arg):
#     htype = ""
#     ctype = ""

class ivectorvectorsize(input_arrayarray):
    htype = "[[Int]]"
    ctype = "CInt"

    def _marshall_in(self):
        n = self.name
        return [f"withArrayArrayIntLen {n} $ \\{n}_nn' {n}_n' {n}' -> do"]

class ivectorvectordouble(input_arrayarray):
    htype = "[[Double]]"
    ctype = "CDouble"

    def _marshall_in(self):
        n = self.name
        return [f"withArrayArrayDoubleLen {n} $ \\{n}_nn' {n}_n' {n}' -> do"]


# output types

class oint(oarg):
    htype = "Int"
    ctype = "CInt"

    indent = 1

    def marshall_in(self):
        n = self.name
        return [f"alloca $ \\{n}' -> do"]

    def marshall_out(self):
        n = self.name
        return [f"{n}'' <- peek {n}'",
                f"let {n}''' = fromIntegral {n}''"]


class osize(oarg):
    htype = "Int"
    ctype = "CInt"

    indent = 1

    def marshall_in(self):
        n = self.name
        return [f"alloca $ \\{n}' -> do"]

    def marshall_out(self):
        n = self.name
        return [f"{n}'' <- peek {n}'",
                f"let {n}''' = fromIntegral {n}''"]

class odouble(oarg):
    htype = "Double"
    ctype = "CDouble"

    indent = 1

    def marshall_in(self):
        n = self.name
        return [f"alloca $ \\{n}' -> do"]

    def marshall_out(self):
        n = self.name
        return [f"{n}'' <- peek {n}'",
                f"let {n}''' = realToFrac {n}''"]

class ostring(oarg):
    htype = "String"
    ctype = "CString"

    indent = 1

    def marshall_in(self):
        n = self.name
        return [f"alloca $ \\{n}' -> do"]

    def marshall_out(self):
        n = self.name
        return [f"{n}'' <- peek {n}'",
                f"{n}''' <- peekCString {n}''"]

class ovectorint(output_array):
    htype = "[Int]"
    ctype = "CInt"

    indent = 2

    def marshall_in(self):
        n = self.name
        return [f"alloca $ \\{n}' -> do",
                f"   alloca $ \\{n}_n' -> do"]

    def marshall_out(self):
        n = self.name
        return [f"{n}'' <- peekArrayInt {n}_n' {n}'"]

    def return_name(self):
        return f"{self.name}''"

class ovectorsize(output_array):
    htype = "[Int]"
    ctype = "CInt"

    indent = 2

    def marshall_in(self):
        n = self.name
        return [f"alloca $ \\{n}' -> do",
                f"   alloca $ \\{n}_n' -> do"]

    def marshall_out(self):
        n = self.name
        return [f"{n}'' <- peekArrayInt {n}_n' {n}'"]

    def return_name(self):
        return f"{self.name}''"

class ovectordouble(output_array):
    htype = "[Double]"
    ctype = "CDouble"

    indent = 2

    def marshall_in(self):
        n = self.name
        return [f"alloca $ \\{n}' -> do",
                f"   alloca $ \\{n}_n' -> do"]

    def marshall_out(self):
        n = self.name
        return [f"{n}'' <- peekArrayDouble {n}_n' {n}'"]

    def return_name(self):
        return f"{self.name}''"

class ovectorstring(output_array):
    htype = "[String]"
    ctype = "CString"

    indent = 2

    def marshall_in(self):
        n = self.name
        return [f"alloca $ \\{n}' -> do",
                f"   alloca $ \\{n}_n' -> do"]

    def marshall_out(self):
        n = self.name
        return [f"{n}'' <- peekArrayString {n}_n' {n}'"]

    def return_name(self):
        n = self.name
        return f"{n}''"

class ovectorpair(output_array):
    htype = "[(Int,Int)]"
    ctype = "CInt"

    indent = 2

    def marshall_in(self):
        n = self.name
        return [f"alloca $ \\{n}' -> do",
                f"   alloca $ \\{n}_n' -> do"]

    def marshall_out(self):
        n = self.name
        return [f"{n}'' <- peekArrayPairs {n}_n' {n}'"]

    def return_name(self):
        return f"{self.name}''"

# Not used
# class ovectorvectorint(ivectorvectorint):
#     output = True

class ovectorvectorsize(output_arrayarray):
    output = True
    htype = "[[Int]]"
    ctype = "CInt"

    indent = 3

    def marshall_in(self):
        n = self.name
        return [f"alloca $ \\{n}' -> do",
                f"   alloca $ \\{n}_n' -> do",
                f"      alloca $ \\{n}_nn' -> do"]

    def marshall_out(self):
        n = self.name
        return [f"{n}'' <- peekArrayArrayInt {n}_nn' {n}_n' {n}'"]

    def return_name(self):
        n = self.name
        return f"{n}''"

class ovectorvectordouble(output_arrayarray):
    output = True
    htype = "[[Double]]"
    ctype = "CDouble"

    indent = 3

    def marshall_in(self):
        n = self.name
        return [f"alloca $ \\{n}' -> do",
                f"   alloca $ \\{n}_n' -> do",
                f"      alloca $ \\{n}_nn' -> do"]

    def marshall_out(self):
        n = self.name
        return [f"{n}'' <- peekArrayArrayDouble {n}_nn' {n}_n' {n}'"]

    def return_name(self):
        n = self.name
        return f"{n}''"

class ovectorvectorpair(output_arrayarray):
    output = True
    htype = "[[(Int, Int)]]"
    ctype = "CInt"

    indent = 3

    def marshall_in(self):
        n = self.name
        return [f"alloca $ \\{n}' -> do",
                f"   alloca $ \\{n}_n' -> do",
                f"      alloca $ \\{n}_nn' -> do"]

    def marshall_out(self):
        n = self.name
        return [f"{n}'' <- peekArrayArrayPairs {n}_nn' {n}_n' {n}'"]

    def return_name(self):
        return f"{self.name}''"

class argcargv(arg):
    output = False

    indent = 1

    def __init__(self, *args):
        self.name = "argv"
        self.htype = None
        self.ctype = None
        self.value = None

    def marshall_in(self):
        n = self.name
        return ["let argc' = fromIntegral $ length argv",
                "withArgv argv $ \\argv' -> do"]

    def foreignexp(self):
        return ["CInt", "Ptr CString"]

    def type_signature(self):
        return ["[String]"]

    def ccall_inputs(self):
        return "argc' argv'"

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
                          self.str_body(prefix),
                          self.str_foreignexp(prefix)])

    def input_arguments(self):
        return [a for a in self.args if not a.output]

    def output_arguments(self):
        return [x for x in self.args if x.output]

    def str_type_signature(self, prefix):
        """ Generate the type signature for the haskell function
        Input variables need to be present, output variables are wrapped
        in the IO action.

        If function has the return_type specified, it will always be the first
        one in IO(...)

        Example:

        gmshModelGetEntities :: Int -> IO([(Int, Int)]) """

        fname = prefix  + camelcasify(self.name)
        inputs = self.input_arguments()
        outputs = self.output_arguments()
        if self.return_type is not None:
            outputs = [self.return_type] + outputs

        itypes = flatten2([a.type_signature() for a in inputs])
        itypesign = " -> ".join(itypes)

        if(len(itypesign) > 0):
            itypesign += " -> "

        otypes = flatten2([a.type_signature() for a in outputs])
        otypesign = ", ".join(otypes)

        return "".join([fname, " :: ",  itypesign, "IO(", otypesign, ")"])

    def str_body(self, prefix):
        """ Generates the body of the function call with marshalling etc.

        some sort of template:

        fname i1 i2 ... in = do
            let ik1 = ...
            ... types which can be converted to C values ...

            withArrayLen $ \ arr1 arr1_n ->
            ... all input types which are delivered using pointers

            alloca $ \ o1 -> do
            ... return types which need allocation

            alloca $ \ errptr -> do
            ... the error pointer

            -- the actual call to the function
            out = cfname ...arguments...

            checkErrorCodeAndThrow errptr

            -- output marshalling
            oval <- out
            let moval = marshaller oval

            -- peeking and marshalling output arguments
            oval1 <- peekX o1
            ...

            -- the actual output argument always first
            return (moval, oval1, oval2, ... ovaln)

        """

        inputs = self.input_arguments()
        outputs = self.output_arguments()
        rtype = self.return_type
        fname = prefix + camelcasify(self.name)

        ## Function declaration line
        decline = [fname]
        for a in inputs:
            decline.append(a.input_name())
        decline.append("= do")
        lines = [" ".join(decline)]

        # this hack is required to make the indentation of the do blocks
        indlevel = 1
        ## Input variables with marshalling
        for a in inputs:
            for l in a.marshall_in():
                lines.append(" "*3*indlevel + l)
            indlevel = a.indent + indlevel

        ## Output variables with marshalling
        for a in outputs:
            for l in a.marshall_in():
                lines.append(" "*3*indlevel + l)
            indlevel = a.indent + indlevel

        ## error ptr
        lines.append(" "*3*indlevel+"alloca $ \\errptr -> do")

        indlevel = indlevel+1
        indent = 3*indlevel*" "


        ## the actual call
        if rtype is None:
            calline = [indent+f"c{fname}"]
        else:
            calline = [indent+f"{rtype.name}'' <- c{fname}"]
        for a in self.args:
            calline.append(a.ccall_inputs())
        # error pointer to the end
        calline.append("errptr")
        lines.append(" ".join(calline))

        ## check error code
        lines.append(indent+f"checkErrorCodeAndThrow \"{fname}\" errptr")

        ## output marshalling
        if rtype is not None:
            # skip the first line in hope that it is enough. (no need to )
            # unwrap pointer as the function call already uses <-
            for l in rtype.marshall_out()[1:]:
                 lines.append(indent + l)

        for ov in outputs:
            for l in ov.marshall_out():
                lines.append(indent + l)

        ## the return line
        if rtype is not None:
            rline = [rtype.return_name()]
        else:
            rline = []

        for ov in outputs:
            rline.append(ov.return_name())

        lines.append(indent + "return (" + ", ".join(rline) + ")")

        return "\n".join(lines)

    def str_foreignexp(self, prefix):
        """
        Generates the foreign import statement for the function, as a string.

        Example:

        foreign import ccall safe "gmshc.h gmshModelGetEntities"
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
        lines = [f"foreign import ccall safe \"gmshc.h {fname}\""]
        lines.append("   {}".format("c"+fname))

        # print arguments (c types), always at least errorcode
        # which is not present in the api definition
        args = list(self.args) + [oint("errcode")]
        types = flatten2([a.foreignexp() for a in args])

        lines.append(f"      :: {types[0]}")
        for t in types[1:]:
            lines.append(f"      -> {t}")

        if self.return_type is None:
            lines.append("      -> IO()")
        else:
            t = self.return_type.type_in_return()[0]
            lines.append(f"      -> IO({t})")

        return "\n".join(lines)

class Module:

    def __init__(self, name, doc):
        self.name = name
        self.doc = doc
        self.fs = []
        self.submodules = []

    def add(self, name, doc, rtype, *args):
        # let's name the output variable oval to ease the process
        if rtype is not None:
            rtype = rtype("oval")
        self.fs.append(Function(rtype, name, args, doc, []))

    def add_special(self, name, doc, special, rtype, *args):
        if "onlycc++" in special:
            # skip things meant only for c++
            return
        if rtype is not None:
            rtype = rtype("oval")
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

        for f in self.fs:
            fhandle.write(f.to_string(prefix))
            fhandle.write("\n")

        for m in self.submodules:
            m.write_module(fhandle, **fwd_kwargs)



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
        # this should get called from haskell-gmsh/apigenerator/
        with open("../src/GmshAPI.hs", 'w') as f:
            vmaj = self.version_major
            vmin = self.version_minor
            head = ("{- \n"
                    "GmshAPI.hs - GMSH C API for Haskell. \n"
                    f"Api version: {vmaj}.{vmin}")
            f.write(head)
            f.write(haskell_header)
            for m in self.modules:
                m.write_module(f)


haskell_header = """
GmshAPI.hs - GMSH C API.
Copyright (C) 2019  Antero Marjamäki

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License ("LICENSE" file) for more details.

This file is automatically generated. DO NOT EDIT DIRECTLY!
-}

module GmshAPI where

import Control.Monad (liftM)
import Foreign.C -- get the C types
import Foreign.Ptr (Ptr,nullPtr)
import Foreign.C.String (withCString, peekCString)
import Foreign.Marshal (alloca, fromBool)
import Foreign.Marshal.Utils (withMany)
import Foreign.Marshal.Array (withArray, peekArray, advancePtr, withArrayLen)
import Foreign.Storable (peek, Storable)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

toInt :: Ptr CInt -> IO Int
toInt = liftM fromIntegral . peek

-- marshaller for argv type "char ** argv"
withArgv :: [String] -> (Ptr CString -> IO a) -> IO a
withArgv ss f = withMany withCString ss f'
   where
      f' x = withArray x f


withArrayArrayLen
   :: (Storable a )
   => [[a]]
   -> (CInt -> Ptr CInt -> Ptr (Ptr a) -> IO(b))
   -> IO (b)
withArrayArrayLen arr f = do
   let len = fromIntegral $ length arr
   let lens = map fromIntegral $ map length arr
   withMany withArray arr $ \\marr -> do
      -- marr :: [Ptr a]
      withArray marr $ \\mmarr -> do
         --mmarr :: Ptr (Ptr a), toivottavasti
         withArray lens $ \\larr -> do
            f len larr mmarr

withArrayArrayIntLen
   :: [[Int]]
   -> (CInt -> Ptr CInt -> Ptr (Ptr CInt) -> IO(b))
   -> IO (b)

withArrayArrayIntLen arr =
   let arr' = map (map fromIntegral) arr
   in withArrayArrayLen arr'

withArrayArrayDoubleLen
   :: [[Double]]
   -> (CInt -> Ptr CInt -> Ptr (Ptr CDouble) -> IO(b))
   -> IO (b)

withArrayArrayDoubleLen arr =
   let arr' = map (map realToFrac) arr
   in withArrayArrayLen arr'

withArrayIntLen :: [Int] -> (CInt -> Ptr CInt -> IO(b)) -> IO(b)
withArrayIntLen arr f =
    let arr' = map fromIntegral arr
        f' len ptr = f (fromIntegral len) ptr
    in withArrayLen arr' f'


withArrayPairLen
    :: [(Int, Int)]
    -> (CInt -> Ptr CInt -> IO(b))
    -> IO(b)

withArrayPairLen arr f =
    let arr' = map fromIntegral $ pairsToFlat arr
    in
        withArrayLen arr' $ \\narr arr'' -> do
            f (fromIntegral narr) arr''

withArrayDoubleLen :: [Double] -> (CInt -> Ptr CDouble -> IO(b)) -> IO(b)
withArrayDoubleLen arr f =
    let arr' = map realToFrac arr
        f' len ptr = f (fromIntegral len) ptr
    in withArrayLen arr' f'

withArrayStringLen :: [String] -> (CInt -> Ptr CString -> IO(b)) -> IO(b)
withArrayStringLen strs f = do
    let len = fromIntegral $ length strs
    withMany withCString strs $ \\marr -> do
        -- marr :: [CString]
        withArray marr $ \\mmarr -> do
            -- mmarr :: Ptr CString hopefully!
            f len mmarr

peekInt :: Ptr CInt -> IO(Int)
peekInt = liftM fromIntegral . peek

peekArrayString :: Ptr CInt -> Ptr (Ptr CString) -> IO([String])
peekArrayString nptr arrptr = do
    nstrs <- peekInt nptr
    arr <- peek arrptr
    strings <- peekArray nstrs arr
    sequence $ map peekCString strings


flatToPairs :: [a] -> [(a,a)]
flatToPairs [] = []
flatToPairs (x:y:[]) = [(x,y)]
flatToPairs (x:y:xs) = (x,y) : flatToPairs xs

pairsToFlat :: [(a,a)] -> [a]
pairsToFlat lst = reverse $ foldl (\\acc (a,b) -> b:a:acc) [] lst


-- Ptr (Ptr CInt) is a serialized list of integers, i.e.
-- **int is a pointer to an array, not an array of arrays... D'OH!
peekArrayPairs :: Ptr CInt -> Ptr (Ptr CInt) -> IO([(Int, Int)])
peekArrayPairs nptr arrptr  = do
  npairs <- peekInt nptr
  arr <- peek arrptr
  flatpairs <- peekArray npairs arr
  return $ flatToPairs $ map fromIntegral flatpairs

peekArrayInt :: Ptr CInt -> Ptr (Ptr CInt) -> IO([Int])
peekArrayInt nptr arrptr  = do
  nints <- peekInt nptr
  arr <- peek arrptr
  ints <- peekArray nints arr
  return $ map fromIntegral ints

peekArrayDouble :: Ptr CInt -> Ptr (Ptr CDouble) -> IO([Double])
peekArrayDouble nptr arrptr  = do
    nints <- peekInt nptr
    arr <- peek arrptr
    ints <- peekArray nints arr
    return $ map realToFrac ints


peekArrayArray
    :: (Storable a)
    => ([a] -> [b])
    -> Ptr CInt
    -> Ptr (Ptr CInt)
    -> Ptr (Ptr (Ptr a))
    -> IO ([[b]])
-- Peeks a nested array and uses f to map the
-- result to correct datatype
peekArrayArray f nnPtr nPtr arrPtrPtr  =
  do
    nn <- peekInt nnPtr
    narr <- peek nPtr
    lens <- peekArray nn narr
    arrPtr <- peek arrPtrPtr
    -- okay, so. fold over the list of lengths, lens.
    -- For each element dereference the pointer
    -- then peek n elements from the array, then advance the outer pointer
    -- accumulate the list of peeked lists, and the advanced pointer
    (lists,_) <- foldl foldfun (return ([], arrPtr)) $ map fromIntegral lens
    return lists

  where
    -- foldfun takes the previous IO action and runs it,
    -- then proceeds to peek and advance ptrs and maps the
    -- result using f
    foldfun action n = do
        (acc, ptr) <- action
        aptr <- peek ptr
        lst <- peekArray n aptr
        let out = f lst
        --let pairss = flatToPairs $ map fromIntegral lst
        let newptr = advancePtr ptr 1
        return ((out:acc), newptr)

peekArrayArrayInt
  :: Ptr CInt
  -> Ptr (Ptr CInt)
  -> Ptr (Ptr (Ptr CInt))
  -> IO([[Int]])
peekArrayArrayInt = peekArrayArray $ map fromIntegral

peekArrayArrayDouble
  :: Ptr CInt
  -> Ptr (Ptr CInt)
  -> Ptr (Ptr (Ptr CDouble))
  -> IO([[Double]])
peekArrayArrayDouble = peekArrayArray $ map realToFrac

peekArrayArrayPairs
  :: Ptr CInt
  -> Ptr (Ptr CInt)
  -> Ptr (Ptr (Ptr CInt))
  -> IO([[(Int, Int)]])
peekArrayArrayPairs  = peekArrayArray $ flatToPairs . map fromIntegral

checkErrorCodeAndThrow :: String -> Ptr CInt -> IO()
checkErrorCodeAndThrow funname errptr = do
  errcode <- peekInt errptr
  if errcode == 0
      then return ()
      else error $ funname ++ " returned nonzero error code: " ++ show errcode

"""
