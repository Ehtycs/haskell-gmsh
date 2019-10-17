# haskell-gmsh

This module implements Haskell bindings to Gmsh C API. It's a work in progress.

### Update 17.10.2019
The generator and marshallers are mostly in place. The generator is working and the generated code with marshallers also compiles. Next things need to be tested. Some form of automated testing, at least
for the basic functionality would be nice.

### Update 22.9.2019
The best-ish approach seems to be to manually write marshalling functions which can be used together with the modified GMSH API generator. Since all types are built-in types, it is hopefully simpler than using FFI generating tools and gives more control on how to handle e.g. errors and default arguments.

# Notes and pitfalls

## GMSH library and C-header
GMSH is now imported from a system location, if you have it in some unorthodox location, add following lines to your "~/.stack/config.yaml".

```
extra-lib-dirs:
    - <path-to-lib-dir>
extra-include-dirs:
    - <path-to-include-dir>
```

## Run GUI functions only in main thread
Ghci by default executes computations in separate threads. Gmsh's FLTK GUI requires that certain C functions need to be called only on the main thread.

In order to invoke the GUI (gmshFltk*) run ghci with "-fno-ghci-sandbox" flag.
https://stackoverflow.com/questions/14307789/cant-get-wxhaskell-to-work-from-ghci-on-mac
