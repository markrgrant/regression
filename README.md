A library for performing multiple linear regression on a list of data.

Two different approaches are taken.  The first uses Repa and the second
uses the HMatrix library (which in turn uses LAPACK).

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


## Performing Regression

1. Populate a list of type [a], where each row is a list of inputs.  
2. Create a list of feature functions of type (a -> Double), where each
   function takes a row of input and returns the value of the feature.
3. Create a 
3. Decide on the method to be used for optimization.  One option is iterative
   gradient descent.  Another 
