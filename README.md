# cray_reveal
Examples showing how to use Cray reveal to inspect code optimization

## Overview
To achieve best execution performance, it is common for compilers to modify code. Many code transformations are at loop levels and these include:

 * Loop unrolling. This replicates the body of a loop in order to decrease the number of times a loop condition is computed. The effect is to reduce the loop overhead compared to computation. 
 * Loop fusion. This combines multiple loops into a single loop. The effect is to reduce the loop overhead. 
 * Loop parallelization. Different loop iterations are executed by different threads simultaneously. 
 * Loop vectorization. Multiple loop itereates are executed simultaneously.

The above transformations cannot be applied when the result is dependent on the ordering of the loop iterations. The Cray ```reveal``` tool can help you identify which optimization strategies are used in which parts of your program. There are cases where the compiler might be too conservative -- the programmer can then provide directives to the compiler to indicate that it is safe to optimize.  


## How to use reveal

The ```reveal``` tool works only for the ```PrgEnv-cray``` programming environment. You will also need to load the CPU architecture:
```
module load craype-x86-skylake
```

You will need to compile your code (whether Fortran, C or C++) using the ```-h -pl=<perf_listing>```. For instance:

```
rm -rf foo.pl
ftn -h -pl=foo.pl foo.f90
```

Then use the ```reveal``` tool to inspect the perfromance listing file:
```
reveal foo.pl
```
(There is no need to run the code.)

## Walking through an example

Our example code advects a pulse in three dimensions using a first order upwind scheme. The code takes the number of cells in in x, y and z.  We build the code using
```
module load slurm craype-x86-skylake perftools
ftn -c upwind_mod.F90
ftn -c upwind_main.F90
ftn -o upwind upwind_mod.o upwind_main.o
```

To get baseline performance data, we type:
```
pat_build upwind
```
This will create executable ```upwind+pat```. 

We can run the the non-instrumented and the instrumented versions of the code with 
```
sbatch upwind.sl
sbatch upwind+pat.sl
```

To obtain an instrumentation report, type: 
```
pat_report upwind+pat+N-Ns > report-pat.txt 
```
where upwind+pat+N-Ns is the name of directory created when running upwind+pat (N are some numbers that will change with each run).  The output of the 
slurm command should look like:
```
number of threads:          1 max number of threads:          1
number of cells:        256        256        256
number of time steps:        256
check sum:     1.000000000
Experiment data directory written:
/scale_akl_nobackup/filesets/transit/pletzera/cray_reveal/code/upwind+pat+56467-61s

real    3m12.688s
user    0m0.020s
sys     0m0.008s
```

The performance report shows that XX percent of the time is spent subroutine YYY. 
```
```

Change your directory to ```code/``` and 
```
rm -rf upwind.pl
ftn -h -pl=upwind.pl upwind.F90
```

Then run
```
reveal upwind.pl
```

Click on ```upwind.F90``` and scroll down to line 122. This is the main loop, ```ntot``` is the number of cells. 

## Final words
