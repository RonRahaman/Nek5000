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
  message (FATAL_ERROR "For Nek5000, you must provide the CASENAME cache variable when running \
cmake (e.g. cmake -D CASENAME=my_casename Nek5000/)")
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

# === Tweak SIZE file =============================================================================

set (SIZE_FILE "${CMAKE_CURRENT_BINARY_DIR}/SIZE")
message("-- Configuring SIZE file: ${SIZE_FILE}")
execute_process(
  COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/bin/config_size.sh" "${SIZE_FILE}" 
  #  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  RESULT_VARIABLE   CONFIG_SIZE_RETURN_VAL
  OUTPUT_VARIABLE   CONFIG_SIZE_STDERR
  ERROR_VARIABLE    CONFIG_SIZE_STDERR
  )
if (NOT CONFIG_SIZE_RETURN_VAL EQUAL 0) 
  message(FATAL_ERROR "Could not configure SIZE file: ${SIZE_FILE}\n${CONFIG_SIZE_STDERR}")
endif ()

# === Tweak .usr file =============================================================================

set (USR_FILE "${CMAKE_CURRENT_BINARY_DIR}/${CASENAME}.usr")
message("-- Configuring .usr file: ${USR_FILE}")
execute_process(
  COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/bin/config_usr.sh" "${USR_FILE}" 
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  RESULT_VARIABLE   CONFIG_USR_RETURN_VAL
  OUTPUT_VARIABLE   CONFIG_USR_STDERR
  ERROR_VARIABLE    CONFIG_USR_STDERR
  )
if (NOT CONFIG_USR_RETURN_VAL EQUAL 0) 
  message(FATAL_ERROR "Could not configure .usr file: ${USR_FILE}\n${CONFIG_USR_STDERR}")
endif ()

# === Source Files ================================================================================

# The source files are separated into groups.  Each group will require a different set of compiler
# flags and/or dependencies.  See the "Compiler Flags" and "Targets and Dependencies" sections
# below.

# Core FORTRAN source
set (NEK_CORE_F_SRC 
  core/3rd_party/nek_in_situ.f
  core/bdry.f
  core/byte_mpi.f
  core/calcz.f
  core/coef.f
  core/conduct.f
  core/connect1.f
  core/connect2.f
  core/convect.f
  core/cvode_driver.f
  core/drive1.f
  core/drive2.f
  core/dssum.f
  core/edgec.f
  core/eigsolv.f
  core/fast3d.f
  core/fasts.f
  core/gauss.f
  core/genbox.f
  core/genxyz.f
  core/gfdm_op.f
  core/gfdm_par.f
  core/gfdm_solve.f
  core/gmres.f
  core/hmholtz.f
  core/hsmg.f
  core/ic.f
  core/induct.f
  core/makeq.f
  core/makeq_aux.f
  core/map2.f
  core/mvmesh.f
  core/mxm_wrapper.f
  core/navier0.f
  core/navier1.f
  core/navier2.f
  core/navier3.f
  core/navier4.f
  core/navier5.f
  core/navier6.f
  core/navier7.f
  core/navier8.f
  core/papi.f
  core/perturb.f
  core/plan4.f
  core/planx.f
  core/postpro.f
  core/prepost.f
  core/readat_new.f
  core/speclib.f
  core/ssolv.f
  core/subs1.f
  core/subs2.f
  core/turb.f
  core/vprops.f
  ${CMAKE_CURRENT_BINARY_DIR}/${CASENAME}.f
)

# Math source
set (NEK_MATH_SRC core/math.f)

# BLAS/LAPACK source
if (VENDOR_BLAS OR BGQ)
  set (NEK_BLAS_SRC "")
else ()
  set (NEK_BLAS_SRC 
    core/3rd_party/blas.f 
    core/3rd_party/dsygv.f 
    core/3rd_party/ssygv.f
  )
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
if (IFMPI)
  set (NEK_MPI_SRC core/byte_mpi.f core/comm_mpi.f)
else ()
  set (NEK_MPI_SRC ${CMAKE_CURRENT_BINARY_DIR}/mpif.h core/byte_mpi.f core/comm_mpi.f core/mpi_dummy.f)
endif ()

# MXM source
if (BGQ)
  set (NEK_MXM_SRC core/mxm_bgq.f core/mxm_std.f)
else ()
  set (NEK_MXM_SRC core/mxm_std.f)
endif ()

# NEKNEK source
if (NEKNEK)
  set (NEK_NEKNEK_SRC core/multimesh.f)
else ()
  set (NEK_NEKNEK_SRC core/singlmesh.f)
endif ()

# Core C source
set (NEK_CORE_C_SRC 
  core/3rd_party/dictionary.c
  core/3rd_party/finiparser.c
  core/3rd_party/iniparser.c 
  core/byte.c 
  core/chelpers.c 
  core/nek_comm.c 
)

# JL core source
set (JL_CORE_SRC
  jl/comm.c
  jl/crystal.c
  jl/fail.c
  jl/fcrystal.c
  jl/gs.c
  jl/gs_local.c
  jl/sarray_sort.c
  jl/sarray_transfer.c
  jl/sort.c
  jl/tensor.c
)

# JL interpolation source
set (JL_INTERP_SRC 
  jl/findpts.c 
  jl/findpts_el_2.c
  jl/findpts_el_3.c 
  jl/findpts_local.c 
  jl/lob_bnd.c 
  jl/obbox.c 
  jl/poly.c 
)

# JL conjugate gradient solver source
if (AMG)
  set (JL_CGS_SRC jl/amg.c jl/fcrs.c)
else ()
  set (JL_CGS_SRC jl/fcrs.c jl/sparse_cholesky.c jl/xxt.c)
endif ()

# All the source files
set (NEK_LIB_SRC
  "${NEK_CORE_F_SRC};${NEK_MATH_SRC};${NEK_BLAS_SRC};${NEK_CMT_SRC};${NEK_MPI_SRC};${NEK_MXM_SRC};\
${NEK_NEKNEK_SRC};${NEK_CORE_C_SRC};${JL_CORE_SRC};${JL_INTERP_SRC};${JL_CGS_SRC}"
)
set (NEK_EXEC_SRC "core/drive.f;${NEK_LIB_SRC}")

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

# === Targets and Dependencies  ==================================================================

set(NEK_LIB "nek5000_${CASENAME}")
add_library("${NEK_LIB}" SHARED ${NEK_LIB_SRC})

add_custom_target(
  RM_MPIF_H
  COMMAND echo "-- DELETING mpif.h "
  COMMAND rm -rf ${CMAKE_CURRENT_BINARY_DIR}/mpif.h
)

add_custom_target(
  CP_MPI_DUMMY_H
  COMMAND echo "-- COPYTING mpi_dummy.h --> mpif.h "
  COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/core/mpi_dummy.h ${CMAKE_CURRENT_BINARY_DIR}/mpif.h
)

if (IFMPI)
  add_dependencies("${NEK_LIB}" RM_MPIF_H)
else ()
  add_dependencies("${NEK_LIB}" CP_MPI_DUMMY_H)
endif ()

include_directories("${CMAKE_CURRENT_BINARY_DIR}" core/ jl/)

if (CMTNEK)
  include_directories(core/cmt)
endif ()

add_executable(nek5000 core/drive.f)
target_link_libraries(nek5000 "${NEK_LIB}")
