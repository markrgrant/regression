A library for performing multiple linear regression on a list of data using
Repa for better multicore performance. 

TODO: 
* performance analysis comparison of different backends (native arrays,
    Repa, Accelerate)
* support an Accelerate backend
* replace the use of lists in the interface with vectors
* replace the use of strings with text
* verify appropriate compilation and runtime options are enabled and 
  support optimal multi-core utilization.

Notes:

The parameters to pass to ghc during compilation and execution with a
repa on a quad core system:
$ ghc -O2 --make -threaded -rtsopts source.hs
$ ./repa "haskell.jpg" +RTS -N4
