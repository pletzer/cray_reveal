# cray_reveae
Examples showing how to use Cray reveal to inspect code optimization

## Overview
To achieve best execution performance, it is common for compilers to modify code. Many code transformations are at loop levels and these include:

 * Loop unrolling. This replicates the body of a loop in order to decrease the number of times a loop condition is computed. The effect is to reduce the loop overhead compared to computation. 
 * Loop fusion. This combines multiple loops into a single loop. The effect is to reduce the loop overhead. 
 * Loop parallelization. Different loop iterations are executed by different threads simultaneously. 
 * Loop vectorization. Multiple loop itereates are executed simultaneously.

The above transformations cannot be applied when the result is dependent on the ordering of the loop iterations. The Cray ```reveal``` tool can help you identify which optimization strategies are used in which parts of your program. There are cases where the compiler might be too conservative -- the programmer can then provide directives to the compiler to indicate that it is safe to optimize.  


## How to use reveal

The ```reveal``` tool works only for the ```PrgEnv-cray``` programming environment. You will also need to load:
```
module load perftools
```

You will need to compile your code (whether Fortran, C or C++) using the ```-h -pl=<perf_listing>```. For instance:

```
ftn -h -pl=foo.pl foo.f90
```

Then use the ```reveal``` tool to inspect the perfromance listing file:
```
reveal foo.pl
```
(There is no need to run the code.)

## Walking through an example

## Final words
