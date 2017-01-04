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
endif ()

if (IFAMG_DUMP AND NOT IFAMG)
  message ("-- Ignoring IFAMG_DUMP=ON since IFAMG=OFF")
endif ()

# === Preprocessor Symbols =====================================================

add_definitions(-DGLOBAL_LONG_LONG -DPREFIX=jl_)

if (IFMPI)
  add_definitions (-DMPI)
endif ()

if (IFAMG AND IFAMG_DUMP)
  add_definitions (-DAMG_DUMP)
endif ()

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
endif ()

if (XSMM)
  add_definitions(-DXSMM)
endif ()

if (CVODE)
  add_definitions(-DCVODE)
endif ()

if (VENDOR_BLAS)
  add_definitions(-DVENDOR_BLAS)
endif ()

if (EXTBAR)
  add_definitions(-DEXTBAR)
endif ()

if (NEKNEK)
  add_definitions(-DNEKNEK)
endif ()

if (CMTNEK)
  add_definitions(-DCMTNEK)
endif ()

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

# Fortran name mangling
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

# === Tweak SIZE file ==========================================================
# TODO: Tweak SIZE file

# === Tweak .usr file ==========================================================
# TODO: Tweak .usr file

# === Makefile targets =========================================================

# Core FORTRAN source
set (NEK_CORE_F_SRC drive1.f drive2.f plan4.f bdry.f coef.f conduct.f connect1.f connect2.f dssum.f
  edgec.f eigsolv.f gauss.f genxyz.f navier1.f makeq.f navier0.f navier2.f navier3.f navier4.f
  prepost.f speclib.f map2.f turb.f mvmesh.f ic.f ssolv.f planx.f mxm_wrapper.f hmholtz.f
  gfdm_par.f  gfdm_op.f gfdm_solve.f subs1.f subs2.f genbox.f gmres.f hsmg.f convect.f induct.f
  perturb.f navier5.f navier6.f navier7.f navier8.f fast3d.f fasts.f calcz.f byte_mpi.f postpro.f
  cvode_driver.f vprops.f qthermal.f cvode_aux.f makeq_aux.f papi.f nek_in_situ.f readat_new.f
  ${CASENAME}.f)

# Math source
set (NEK_MATH_SRC "math.f")

# BLAS/LAPACK source
if (VENDOR_BLAS OR BGQ)
  set (NEK_BLAS_SRC "")
else ()
  set (NEK_BLAS_SRC blas.f dsygv.f ssygv.f)
endif ()

# CMT source
if (CMTNEK)
  set (NEK_CMT_SRC drive1_cmt.f drive2_cmt.f driver3_cmt.f face.f ausm.f  MixtPerf.f intpdiff.f
    diffusive_cmt.f eqnsolver_cmt.f surface_fluxes.f bc.f inflow_bc.f outflow_bc.f wall_bc.f step.f
    filters_cmt.f diagnostics.f)
else ()
  set (NEK_CMT_SRC "")
endif ()

# MPI source
# TODO: Handle creation/deletion of mpif.h
if (MPI)
  set (NEK_MPI_SRC byte_mpi.f comm_mpi.f)
else ()
  set (NEK_MPI_SRC byte_mpi.f comm_mpi.f mpi_dummy.f)
endif ()

# MXM source
if (BGQ)
  set (NEK_MXM_SRC mxm_std.f mxm_bgq.f)
else ()
  set (NEK_MXM_SRC mxm_std.f)
endif ()

# NEKNEK source
if (NEKNEK)
  set (NEK_NEKNEK_SRC multimesh.f)
else ()
  set (NEK_NEKNEK_SRC singlmesh.f)
endif ()

# Core C source
set (NEK_CORE_C_SRC byte.f chelpers.f nek_comm.f finiparser.f iniparser.f dictionary.f)

# JL core source
set (JL_CORE_SRC gs.f sort.f sarray_transfer.f sarray_sort.f gs_local.f crystal.f comm.f tensor.f fail.f fcrystal.f)

# JL interpolation source
set (JL_INTERP_SRC findpts.f findpts_local.f obbox.f poly.f lob_bnd.f findpts_el_3.f findpts_el_2.f)

# JL conjugate gradient solver source
if (AMG)
  set (JL_CGS_SRC amg.f fcrs.f)
else ()
  set (JL_CGS_SRC sparse_cholesky.f xxt.f fcrs.f)
endif ()

add_executable(nek5000 core/drive.f)

# === Compiler Flags =================================================

# Compiler-specific FFLAGs
if (CMAKE_Fortran_COMPILER_ID STREQUAL GNU) 
  set (COMP_SPEC_FFLAGS "-fdefault-real-8 -fdefault-double-8 -cpp")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  set (COMP_SPEC_FFLAGS "-r8 -fpconstant -fpp")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL PGI)
  set (COMP_SPEC_FFLAGS "-r8 -Mpreprocess")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL XL)
  set (COMP_SPEC_FFLAGS "-qrealsize=8 -qdpc=e -qsuffix=cpp=f")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL Cray)
  set (COMP_SPEC_FFLAGS "-s default64 -eF")
endif ()

# TODO: Create custom "optimized" and "BGQ" build mode.  See:
# https://cmake.org/Wiki/CMake_FAQ#How_can_I_extend_the_build_modes_with_a_custom_made_one_.3F

# Default (none) build mode
set (CMAKE_C_FLAGS "-O2")
set (CMAKE_Fortran_FLAGS "${COMP_SPEC_FFLAGS} -O2")
set_source_files_properties ("${NEK_BLAS_SRC}" PROPERTIES CMAKE_Fortran_FLAGS "${COMP_SPEC_FFLAGS} -O0" )
set_source_files_properties ("${NEK_MATH_SRC}" PROPERTIES CMAKE_Fortran_FLAGS "${COMP_SPEC_FFLAGS} -O3" )
set_source_files_properties ("${NEK_MXM_SRC}"  PROPERTIES CMAKE_Fortran_FLAGS "${COMP_SPEC_FFLAGS} -O3" )

# Debug build mode
set (CMAKE_C_FLAGS_DEBUG "-g -O2")
set (CMAKE_Fortran_FLAGS_DEBUG "${COMP_SPEC_FFLAGS} -g -O2")
set_source_files_properties ("${NEK_BLAS_SRC}" PROPERTIES CMAKE_Fortran_FLAGS_DEBUG "${COMP_SPEC_FFLAGS} -g -O0" )
set_source_files_properties ("${NEK_MATH_SRC}" PROPERTIES CMAKE_Fortran_FLAGS_DEBUG "${COMP_SPEC_FFLAGS} -O3" )
set_source_files_properties ("${NEK_MXM_SRC}"  PROPERTIES CMAKE_Fortran_FLAGS_DEBUG "${COMP_SPEC_FFLAGS} -O3" )
