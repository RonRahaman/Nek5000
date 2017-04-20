
Took this commit
e62804ff887485a16d4a3dfc81b5eac57602e379

and rebased on top of this commit
62aba6d5efdc0203f4c7199a521c81a82e289673

The last stable commit (wrt. NekTests.py) was SVN r1074:
92b9d5faed48798cbfb4c1b42edcbf546e7615cf



This will allow you to run the bisection with NekTests.  

## Setting up your repos for bisection

First, run the following routines

```
mkdir -p $HOME/nek-bisect/
cd $HOME/nek-bisect/

git clone -b master              https://github.com/Nek5000/Nek5000.git     Nek5000-master
git clone -b rebase-for-bisect-7 https://github.com/RonRahaman/Nek5000.git  Nek5000
git clone                        https://github.com/Nek5000/NekExamples.git NekExamples && \
  cd NekExamples && \
  git checkout adb923dd1707fd0f790291293b62b399147505e3 && \
  cd -

export CC="mpicc"
export F77="mpif77"
export IFMPI="true"
export SOURCE_ROOT="$HOME/nek-bisect/Nek5000"
export TOOLS_ROOT="$HOME/nek-bisect/Nek5000-master/tools"
export EXAMPLES_ROOT="$HOME/nek-bisect/NekExamples"
export PYTHONPATH="$EXAMPLES_ROOT:$PYTHONPATH"
export PATH="$EXAMPLES_ROOT:$PATH"
```

## Running a bisection

Then, to start a bisection (e.g., for the `Pipe_Helix.test_PnPn_Parallel` test):

```
cd $HOME/nek-bisect/Nek5000
git bisect start HEAD 92b9d5faed48798cbfb4c1b42edcbf546e7615cf --
git bisect run python -m 'unittest' NekTests.Pipe_Helix.test_PnPn_Parallel
git bisect reset
```

Alternately, you can use the script:
``
./nek-bisect.sh Pipe_Helix.test_PnPn_Parallel
``


# Nek5000 
![https://travis-ci.org/Nek5000/Nek5000](https://travis-ci.org/Nek5000/Nek5000.svg?branch=develop)
[![GPLv3 licensed](https://img.shields.io/badge/license-GPLv3-blue.svg)](https://raw.githubusercontent.com/Nek5000/nek5000/develop/LICENSE)

Nek5000 is an open source, highly scalable and portable spectral element code
designed to simulate:
* unsteady Stokes
* unsteady incompressible Navier-Stokes
* low Mach-number flows
* heat transfer and species transport
* incompressible magnetohydrodynamics (MHD)

Written in Fortran 77 and C, it relies on MPI for parallelization and on a
subset of LAPACK routines for eigenvalue computations, depending on the
particular solver employed.  Nek5000 output formats can be read by the parallel
visualization package VisIt developed at LLNL/LBNL. VisIt is mandatory for
large problems (e.g. more than 100,000 spectral elements).


## Features

* Scales to over a million processes
* High-order spatial discretization using spectral elements
* High-order semi-implicit timestepping
* Incompressible + low Mach number (variable density) flows
* Efficient preconditioners (multigrid + scalable coarse grid solves)
* Highly optimized computational kernels (e.g. matrix-matrix multiply)
* Low memory footprint and scalable memory design
* High performance parallel I/O
* ALE / moving meshes and free surface flow
* Accurate Lagrangian particle tracking
* Conjugate fluid-solid heat transfer
* Scalable communication kernels
* Built-in profiling analysis
* Interface to VisIt for parallel data analysis and visualization


## Download

You can download the latest release of nek5000 as 
[a zip](https://github.com/Nek5000/nek5000/archive/master.zip) or 
[a tarball](https://github.com/Nek5000/nek5000/archive/master.tar.gz).

You can also clone the repository with git:
```
git clone https://github.com/Nek5000/nek5000.git -b master
```
or even check it out with svn:
```
svn co https://github.com/Nek5000/nek5000.git/branches/master nek5000
```


## User Guide

For more information, see the [user guide](https://nek5000.mcs.anl.gov/documentation/).


## Notes for users of legacy SVN repo

With the move to Git, the Nek5000 source directories have been reorganized to
improve modularity.  Most of the contents of `trunk/nek/` (including the
all-important `makenek`) are now located in the `core/` directory.  Scripts for
running Nek5000 (such as `nekmpi` and `nekb`) are located in the `bin/`
directory.  Tools such as `genbox` and `genmap` are located in `tools/`.  A
small subset of the examples (useful for quick regression tests) are located in
`short_tests/`.  All examples are kept in a separate repository,
[NekExamples](https://github.com/Nek5000/NekExamples), to keep this one
light-weight. 

Nek5000 works the same way it used to: build cases with `core/makenek` and run them with a script, e.g. `bin/nekmpi`.
