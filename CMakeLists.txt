cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project (nek5000 Fortran C)

option (IFMPI       "enable MPI"                                 ON)
option (IFAMG       "enable AMG coarse grid solver"              OFF)
option (IFAMG_DUMP  "dump AMG setup to file (if AMG is enabled)" OFF)
option (IFPROFILING "enable profiing"                            OFF)
option (MPIIO       "use MPI-IO I/O kernel"                      OFF)
option (BGQ         "use BGQ optimized mxm"                      OFF)
option (XSMM        "use libxsmm for mxm"                        OFF)
option (CVODE       "enable CVODE solver for scalars"            OFF)
option (MOAB        "enable MOAB/CUBIT support (experimental)"   OFF)
option (VENDOR_BLAS "link against external BLAS/LAPACK"          OFF)
option (EXTBAR      "adds underscore to exit call (for BGQ)"     OFF)
option (NEKNEK      "YP NekNek solver"                           OFF)
option (CMTNEK      "enable DG compressible-flow solver"         OFF)

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
