A library for performing multiple linear regression on a list of data using
Repa for better multicore performance. An HMatrix implementation is also under
development. 

Much work remains to be done.

TODO: 
* add a tutorial that describes the basic steps for working with the library
* add a helper function that takes a basic feature function, and the list of
  values, and creates either a: normalized feature function, or a scaled
  feature function.
* add a "normalize_all" function that takes a list of values, and a list of
  feature functions to apply to the values, and then scales them using either
  the feature normalizing function above. 
* performance analysis comparison of different backends (native arrays,
    Repa, Accelerate)
* support an Accelerate backend
* replace the use of lists in the interface with vectors
* replace the use of strings with text
* verify appropriate compilation and runtime options are enabled and 
  support optimal multi-core utilization. For example, the parameters to pass
  to ghc during compilation and execution with Repa on a quad core system:

```
$ ghc -O2 --make -threaded -rtsopts source.hs
$ ./source <args> +RTS -N4
```
