# haskell-gmsh

This module implements Haskell bindings to Gmsh C API. It's a work in progress.
Currently the possibility of generating the API definition automatically is
researched.

# Notes and pitfalls

## Run GUI functions only in main thread
Ghci by default executes computations in separate threads. Gmsh's FLTK GUI
requires that certain C functions need to be called only on the main thread.

In order to invoke the GUI (gmshFltk*) run ghci with "-fno-ghci-sandbox" flag.
https://stackoverflow.com/questions/14307789/cant-get-wxhaskell-to-work-from-ghci-on-mac
