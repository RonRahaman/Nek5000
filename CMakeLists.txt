cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project (nek5000 Fortran C)

set (CASENAME    ""  CACHE STRING "name of .usr and .rea/.re2 files for problem")
set (IFMPI       ON  CACHE BOOL   "enable MPI"                                  )
set (IFAMG       OFF CACHE BOOL   "enable AMG coarse grid solver"               )
set (IFAMG_DUMP  OFF CACHE BOOL   "dump AMG setup to file (if AMG is enabled)"  )
set (IFPROFILING OFF CACHE BOOL   "enable profiing"                             )
set (MPIIO       OFF CACHE BOOL   "use MPI-IO I/O kernel"                       )
set (BGQ         OFF CACHE BOOL   "use BGQ optimized mxm"                       )
set (XSMM        OFF CACHE BOOL   "use libxsmm for mxm"                         )
set (CVODE       OFF CACHE BOOL   "enable CVODE solver for scalars"             )
set (MOAB        OFF CACHE BOOL   "enable MOAB/CUBIT support (experimental)"    )
set (VENDOR_BLAS OFF CACHE BOOL   "link against external BLAS/LAPACK"           )
set (EXTBAR      OFF CACHE BOOL   "adds underscore to exit call (for BGQ)"      )
set (NEKNEK      OFF CACHE BOOL   "YP NekNek solver"                            )
set (CMTNEK      OFF CACHE BOOL   "enable DG compressible-flow solver"          )

if (NOT CASENAME)
  message (FATAL_ERROR "For Nek5000, you must provide CASENAME cache variable when running cmake (e.g. cmake -D CASNAME=my_casename)")
endif ()

if (BGQ AND NOT VENDOR_BLAS)
  message ("-- Using vendor's BLAS/LAPACK library for IBM BG/Q")
  set (VENDOR_BLAS ON CACHE BOOL "link against external BLAS/LAPACK" FORCE)
endif ()

# === Preprocessor Symbols =====================================================
if (IFMPI)
  # TODO: handle mpidummy.h
  add_definitions (-DMPI)
endif ()

# TODO: Handle AMG objects in Makefile

if (IFPROFILING)
  if (IFMPI)
    add_definitions (-DTIMER -DMPITIMER)
  else ()
    add_definitions (-DTIMER)
  endif ()
endif ()

if (MPIIO)
  add_definitions(-DMPIIO)
endif ()

if (BGQ)
  add_definitions(-DBGQ)
  # TODO: Handle BGQ mxm objects in Makefile
endif ()

if (XSMM)
  add_definitions(-DXSMM)
endif ()

if (CVODE)
  add_definitions(-DCVODE)
  # TODO: Handle CVODE objects in Makefile
endif ()

if (VENDOR_BLAS)
  add_definitions(-DVENDOR_BLAS)
  # TODO: Handle BLAS objects in Makefile
endif ()

if (EXTBAR)
  add_definitions(-DEXTBAR)
endif ()

if (NEKNEK)
  add_definitions(-DNEKNEK)
  # TODO: Handle NekNek objects in Makefile
endif ()

if (CMTNEK)
  add_definitions(-DCMTNEK)
  # TODO: Handle CMT objects in Makefile
endif ()

add_definitions(-DGLOBAL_LONG_LONG)

# === Type sizes ===============================================================
# Check pointer size
if (CMAKE_C_SIZEOF_DATA_PTR EQUAL 8)
  add_definitions (-DPTRSIZE8)
endif ()

# Check size of long int
include (CheckTypeSize)
check_type_size ("long int" SIZEOF_LONG_INT)
if (SIZEOF_LONG_INT EQUAL 8)
  add_definitions(-DLONGINT8)
endif ()

# === Fortran Name Mangling ====================================================
# After running the FortranCInterface checks, CMake defines a few useful variables:
#   * FortranCInterface_GLOBAL_MACRO:  A preprocessor macro to mangle a Fortran global symbol
#   * FortranCInterface_GLOBAL_PREFIX: Sting to prepend to a Fortran global symbol
#   * FortranCInterface_GLOBAL_SUFFIX: Sting to append to a Fortran global symbol
# For Nek5000, we just need to see if the the suffix is an underscore
include (FortranCInterface)
FortranCInterface_VERIFY ()
if (FortranCInterface_GLOBAL_SUFFIX STREQUAL "_")
  add_definitions (-DUNDERSCORE)
endif ()

# === Compiler-specific Flags =================================================
if (CMAKE_Fortran_COMPILER_ID STREQUAL GNU) 
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8 -fdefault-double-8 -x f77-cpp-input")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -r8 -fpconstant -fpp")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL PGI)
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -r8 -Mpreprocess")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL XL)
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qrealsize=8 -qdpc=e -qsuffix=cpp=f")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL Cray)
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -s default64 -eF")
endif ()

# === Tweak SIZE file ==========================================================
# TODO: Tweak SIZE file

# === Tweak .usr file ==========================================================
# TODO: Tweak .usr file

# === Makefile targets =========================================================
add_executable(nek5000 core/drive.f)
