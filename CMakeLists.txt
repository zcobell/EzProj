# ##############################################################################
# CMake Build File for EZPROJ
#
# Written By: Zach Cobell
#
# ##############################################################################
#
# The CMake build system enable EZPROJ to be deployed and built in a cross
# platform environment.
#
# ##############################################################################
enable_language(C)
enable_language(CXX)
include(CheckIncludeFiles)
include(CheckLibraryExists)
include(CheckFunctionExists)
include(GNUInstallDirs)
include(CMakePackageConfigHelpers)
# ##############################################################################

# ##############################################################################
# DEFAULT BUILD TYPE
# ##############################################################################
if(DEFINED CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE
      ${CMAKE_BUILD_TYPE}
      CACHE STRING "Choose the type of
        build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug
        Release RelWithDebInfo MinSizeRel.")
elseif(COVERAGE)
  set(CMAKE_BUILD_TYPE
      Debug
      CACHE
        STRING
        "Choose the type of build,
            options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release
            RelWithDebInfo MinSizeRel.")
else()
  set(CMAKE_BUILD_TYPE
      Release
      CACHE STRING "Choose the type of build,
        options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release
        RelWithDebInfo MinSizeRel.")
endif()
# ##############################################################################

# ##############################################################################
# GENERAL OPTIONS
# ##############################################################################
cmake_minimum_required(VERSION 2.8.12)
project(EZPROJ)
option(EZPROJ_BUILD_SHARED "Build shared libraries" OFF)
option(EZPROJ_BUILD_FORTRAN "Build Fortran libraries" OFF)
if(EZPROJ_BUILD_SHARED)
  set(EZPROJ_LIBTYPE SHARED)
else(EZPROJ_BUILD_SHARED)
  set(EZPROJ_LIBTYPE STATIC)
endif(EZPROJ_BUILD_SHARED)
if(EZPROJ_BUILD_FORTRAN)
  enable_language(Fortran)
endif(EZPROJ_BUILD_FORTRAN)
# ##############################################################################

# ##############################################################################
# Enable Coverage
# ##############################################################################
# OPTION(COVERAGE "Export Code Coverage report from tests" OFF)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/CMakeModules)
if(COVERAGE)
  if(CMAKE_COMPILER_IS_GNUCXX)
    include(CodeCoverage)
    setup_target_for_coverage(adcmoduels_coverage tests coverage)
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} -Wall -pedantic -pthread -g -O0 -fprofile-arcs -ftest-coverage"
    )
    if(EZPROJ_BUILD_FORTRAN)
      set(CMAKE_Fortran_FLAGS
          "${CMAKE_Fortran_FLAGS} -g -O0 -fprofile-arcs -ftest-coverage")
    endif(EZPROJ_BUILD_FORTRAN)
  endif(CMAKE_COMPILER_IS_GNUCXX)
endif(COVERAGE)
# ##############################################################################

# ##############################################################################
# Enable running tests
# ##############################################################################
if(UNIX OR CYGWIN)
  option(EZPROJ_BUILD_TESTS "Build test cases" OFF)
  enable_testing()
endif(UNIX OR CYGWIN)
# ##############################################################################

# ##############################################################################
# C++ 11 Check
# ##############################################################################
include(CheckCXXCompilerFlag)
check_cxx_compiler_flag("-std=c++11" COMPILER_SUPPORTS_CXX11)
if(COMPILER_SUPPORTS_CXX11)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
else(COMPILER_SUPPORTS_CXX11)
  message(
    FATAL_ERROR
      "The compiler ${CMAKE_CXX_COMPILER} has no C++11 support. Please use a different C++ compiler."
  )
endif(COMPILER_SUPPORTS_CXX11)
# ##############################################################################

# ##############################################################################
# Compiler flags
# ##############################################################################
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
mark_as_advanced(CLEAR CMAKE_CXX_FLAGS_RELEASE)
mark_as_advanced(CLEAR CMAKE_CXX_FLAGS_DEBUG)
mark_as_advanced(CLEAR CMAKE_C_FLAGS_RELEASE)
mark_as_advanced(CLEAR CMAKE_C_FLAGS_DEBUG)
mark_as_advanced(CLEAR CMAKE_CXX_COMPILER)
mark_as_advanced(CLEAR CMAKE_C_COMPILER)
# ##############################################################################

# ##############################################################################
# CODE VERSION (GIT)
# ##############################################################################
execute_process(
  COMMAND git describe --always --tags
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_VERSION
  OUTPUT_STRIP_TRAILING_WHITESPACE)
# ##############################################################################

# ##############################################################################
# LIBRARY VERSION
# ##############################################################################
set(EZPROJ_VERSION_MAJOR 0)
set(EZPROJ_VERSION_MINOR 1)
set(EZPROJ_VERSION_PATCH 0)
set(EZPROJ_VERSION_STRING
    ${EZPROJ_VERSION_MAJOR}.${EZPROJ_VERSION_MINOR}.${EZPROJ_VERSION_PATCH})
# ##############################################################################

# ##############################################################################
# SET THE LOCATION OF TEMPORARY STATIC LIBS
# ##############################################################################
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles)
# ##############################################################################

# ##############################################################################
# Look for system proj4
# ##############################################################################
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
find_package(PROJ4)
if(PROJ4_FOUND)
  message(STATUS "Found System proj4")
  message(STATUS "PROJ4 Header Dir: ${PROJ4_INCLUDE_DIR}")
  message(STATUS "PROJ4 Library: ${PROJ4_LIBRARY}")
else()
  # ############################################################################
  # PROJ Configuration Options
  # ############################################################################
  # ...C options, lifted from Proj4
  if(CMAKE_C_COMPILER_ID STREQUAL "Intel")
    if(MSVC)
      set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /fp:precise")
    else()
      set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fp-model precise")
    endif()
  endif()
  set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
  find_package(Threads)

  include(CheckIncludeFiles)
  include(CheckSymbolExists)
  check_symbol_exists(PTHREAD_MUTEX_RECURSIVE pthread.h
                      HAVE_PTHREAD_MUTEX_RECURSIVE_DEFN)

  include(CheckCSourceCompiles)
  if(MSVC)
    set(CMAKE_REQUIRED_FLAGS "${CMAKE_C_FLAGS} /WX")
  else()
    set(CMAKE_REQUIRED_LIBRARIES m)
    set(CMAKE_REQUIRED_FLAGS "${CMAKE_C_FLAGS} -Werror")
  endif()
  # Check whether the C99 math function: hypot, atanh, etc. are available.
  check_c_source_compiles(
    "#include <math.h>
    int main() {
      int q;
      return (int)(hypot(3.0, 4.0) + atanh(0.8) + cbrt(8.0) +
                   remquo(100.0, 90.0, &q) +
                   remainder(100.0, 90.0) + copysign(1.0, -0.0) +
                   log1p(0.1) + asinh(0.1)) +
                   isnan(0.0);
    }\n"
    C99_MATH)

  # check needed include file
  if(POLICY CMP0074)
    cmake_policy(SET CMP0074 NEW)
  endif(POLICY CMP0074)
  if(POLICY CMP0075)
    cmake_policy(SET CMP0075 NEW)
  endif(POLICY CMP0075)
  check_include_files(dlfcn.h HAVE_DLFCN_H)
  check_include_files(inttypes.h HAVE_INTTYPES_H)
  check_include_files(jni.h HAVE_JNI_H)
  check_include_files(memory.h HAVE_MEMORY_H)
  check_include_files(stdint.h HAVE_STDINT_H)
  check_include_files(stdlib.h HAVE_STDLIB_H)
  check_include_files(string.h HAVE_STRING_H)
  check_include_files(sys/stat.h HAVE_SYS_STAT_H)
  check_include_files(sys/types.h HAVE_SYS_TYPES_H)
  check_include_files(unistd.h HAVE_UNISTD_H)
  check_include_files("stdlib.h;stdarg.h;string.h;float.h" STDC_HEADERS)

  check_function_exists(localeconv HAVE_LOCALECONV)

  # check libm need on unix
  check_library_exists(m ceil "" HAVE_LIBM)

  set(PACKAGE "proj")
  set(PACKAGE_BUGREPORT "https://github.com/OSGeo/proj.4/issues")
  set(PACKAGE_NAME "PROJ")
  set(PACKAGE_STRING "PROJ ${${PROJECT_INTERN_NAME}_VERSION}")
  set(PACKAGE_TARNAME "proj")
  set(PACKAGE_URL "http://proj4.org")
  set(PACKAGE_VERSION "${${PROJECT_INTERN_NAME}_VERSION}")

  configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/proj_config.cmake.in
    ${CMAKE_BINARY_DIR}/CMakeFiles/pj_config/proj_config.h)
  # ############################################################################

  # ############################################################################
  # PROJ
  # ############################################################################
  add_library(
    proj STATIC
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/nad_init.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_aea.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_aeqd.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_airy.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_aitoff.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_august.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_axisswap.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_bacon.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_bipc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_boggs.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_bonne.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_calcofi.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_cart.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_cass.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_cc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_ccon.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_cea.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_chamb.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_collg.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_comill.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_crast.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_deformation.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_denoy.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_eck1.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_eck2.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_eck3.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_eck4.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_eck5.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_eqc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_eqdc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_fahey.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_fouc_s.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_gall.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_geoc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_geos.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_gins8.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_gnom.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_gn_sinu.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_goode.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_gstmerc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_hammer.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_hatano.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_helmert.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_hgridshift.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_horner.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_igh.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_isea.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_imw_p.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_krovak.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_labrd.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_laea.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_lagrng.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_larr.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_lask.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_latlong.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_lcca.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_lcc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_loxim.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_lsat.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_misrsom.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_mbt_fps.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_mbtfpp.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_mbtfpq.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_merc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_mill.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_mod_ster.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_moll.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_molodensky.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_natearth.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_natearth2.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_nell.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_nell_h.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_nocol.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_nsper.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_nzmg.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_ob_tran.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_ocea.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_oea.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_omerc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_ortho.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_patterson.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_pipeline.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_poly.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_putp2.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_putp3.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_putp4p.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_putp5.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_putp6.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_qsc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_robin.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_rpoly.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_sch.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_sconics.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_somerc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_sterea.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_stere.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_sts.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_tcc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_tcea.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_times.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_tmerc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_tpeqd.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_unitconvert.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_urm5.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_urmfps.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_vandg.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_vandg2.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_vandg4.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_vgridshift.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_wag2.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_wag3.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_wag7.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_wink1.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_wink2.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/proj_etmerc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/aasincos.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/adjlon.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/bch2bps.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/bchgen.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/biveval.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/dmstor.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/emess.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/emess.h
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/geocent.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/geocent.h
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/geodesic.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/mk_cheby.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/nad_cvt.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/nad_init.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/nad_intr.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_apply_gridshift.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_apply_vgridshift.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_auth.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_ctx.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_fileapi.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_datum_set.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_datums.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_deriv.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_ell_set.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_ellps.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_errno.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_factors.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_fwd.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_gauss.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_gc_reader.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_geocent.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_gridcatalog.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_gridinfo.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_gridlist.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/PJ_healpix.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_init.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_initcache.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_inv.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_list.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_list.h
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_log.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_malloc.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_math.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_mlfn.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_msfn.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_mutex.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/proj_4D_api.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_internal.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/proj_internal.h
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_open_lib.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_param.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_phi2.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_pr_list.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_qsfn.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_release.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_strerrno.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_transform.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_tsfn.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_units.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_utils.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_zpoly1.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/proj_mdist.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/proj_math.h
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/proj_rouss.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/rtodms.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/vector1.c
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src/pj_strtod.c)

  target_include_directories(
    proj
    PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/proj/proj5.1/src
            ${CMAKE_CURRENT_SOURCE_DIR}/src/
            ${CMAKE_BINARY_DIR}/CMakeFiles/pj_config)

  if(C99_MATH)
    target_compile_definitions(proj PRIVATE "HAVE_C99_MATH=1")
  else()
    target_compile_definitions(proj PRIVATE "HAVE_C99_MATH=0")
  endif()
  if(HAVE_PTHREAD_MUTEX_RECURSIVE_DEFN)
    target_compile_definitions(proj PRIVATE "HAVE_PTHREAD_MUTEX_RECURSIVE=1")
  endif()

  set(PROJ4_LIBRARY proj)
  set(PROJ4_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/proj/proj5.1/src
                        ${CMAKE_BINARY_DIR}/CMakeFiles/pj_config)

endif()
# ##############################################################################

# ##############################################################################
# EZPROJ
# ##############################################################################
add_library(ezproj ${EZPROJ_LIBTYPE} ${CMAKE_CURRENT_SOURCE_DIR}/src/epsg.cpp
                   ${CMAKE_CURRENT_SOURCE_DIR}/src/ezproj.cpp)
target_include_directories(ezproj PRIVATE ${PROJ4_INCLUDE_DIR}
                                          ${CMAKE_CURRENT_SOURCE_DIR}/src)
set(HEADER_LIST ${CMAKE_CURRENT_SOURCE_DIR}/src/ezproj.h)
target_link_libraries(ezproj ${PROJ4_LIBRARY})
if(NOT ${PROJ4_FOUND})
  add_dependencies(ezproj proj)
endif()
set(HEADER_DEST "include")
set_target_properties(ezproj PROPERTIES PUBLIC_HEADER "${HEADER_LIST}")
set_target_properties(ezproj PROPERTIES SOVERSION ${EZPROJ_VERSION_MAJOR}
                                        VERSION ${EZPROJ_VERSION_STRING})
write_basic_package_version_file(
  ezprojConfigVersion.cmake
  VERSION ${EZPROJ_VERSION_STRING}
  COMPATIBILITY SameMajorVersion)
install(
  TARGETS ezproj
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT EZPROJ_RUNTIME
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT EZPROJ_RUNTIME
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT EZPROJ_DEVELOPMENT
  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
                COMPONENT EZPROJ_DEVELOPMENT)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/ezprojConfigVersion.cmake
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake)
# ##############################################################################

# ##############################################################################
# EZPROJ_FORTRAN
# ##############################################################################
if(EZPROJ_BUILD_FORTRAN)
  add_library(
    ezprojf
    ${EZPROJ_LIBTYPE} ${CMAKE_CURRENT_SOURCE_DIR}/src/ezproj_fortran.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/ezproj_fortran.F90)
  target_include_directories(ezprojf PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src)
  add_dependencies(ezprojf ezproj)
  target_link_libraries(ezprojf ezproj)
  set_target_properties(
    ezprojf PROPERTIES Fortran_MODULE_DIRECTORY
                       ${CMAKE_BINARY_DIR}/CMakeFiles/mod/ezprojf)
  if(EZPROJ_BUILD_SHARED)
    install(
      TARGETS ezprojf
      RUNTIME DESTINATION lib
      LIBRARY DESTINATION lib)
  else(EZPROJ_BUILD_SHARED)
    install(
      TARGETS ezprojf
      ARCHIVE DESTINATION lib
      LIBRARY DESTINATION lib)
  endif(EZPROJ_BUILD_SHARED)
  set_target_properties(ezprojf PROPERTIES SOVERSION ${EZPROJ_VERSION_MAJOR}
                                           VERSION ${EZPROJ_VERSION_STRING})
  install(
    TARGETS ezprojf
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT EZPROJ_RUNTIME
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT EZPROJ_RUNTIME
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT EZPROJ_DEVELOPMENT
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
                  COMPONENT EZPROJ_DEVELOPMENT)
  install(
    CODE "EXECUTE_PROCESS (COMMAND \"${CMAKE_COMMAND}\" -E copy_directory \"${CMAKE_BINARY_DIR}/CMakeFiles/mod/ezprojf\" \"${CMAKE_INSTALL_PREFIX}/include\")"
  )
endif(EZPROJ_BUILD_FORTRAN)
# ##############################################################################

# ##############################################################################
# Test Cases
# ##############################################################################
if(UNIX OR CYGWIN)
  if(EZPROJ_BUILD_TESTS)
    # ...C++ Testing
    file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/cxx_tests)

    set(TEST_LIST cxx_toUtm15.cpp cxx_toGeographic.cpp cxx_cpp.cpp
                  cxx_invcpp.cpp)

    foreach(TESTFILE ${TEST_LIST})
      get_filename_component(TESTNAME ${TESTFILE} NAME_WE)
      add_executable(${TESTNAME}
                     ${CMAKE_SOURCE_DIR}/testing/cxx_tests/${TESTFILE})
      add_dependencies(${TESTNAME} ezproj)
      target_include_directories(${TESTNAME} PRIVATE ${CMAKE_SOURCE_DIR}/src)
      target_link_libraries(${TESTNAME} ezproj)
      set_target_properties(
        ${TESTNAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY
                               ${CMAKE_BINARY_DIR}/cxx_tests)

      add_test(
        NAME TEST_${TESTNAME}
        COMMAND ${CMAKE_BINARY_DIR}/cxx_tests/${TESTNAME}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/testing)
      if(CYGWIN)
        set_tests_properties(
          TEST_${TESTNAME} PROPERTIES ENVIRONMENT
                                      "PATH=$ENV{PATH}:${CMAKE_BINARY_DIR}")
      else(CYGWIN)
        set_tests_properties(
          TEST_${TESTNAME}
          PROPERTIES ENVIRONMENT
                     "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_BINARY_DIR}"
        )
      endif(CYGWIN)
    endforeach()

    # ...Fortran Testing
    if(EZPROJ_BUILD_FORTRAN)
      file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/fortran_tests)

      set(TEST_LIST f90_toUtm15.F90 f90_toGeographic.F90 f90_cpp.F90
                    f90_invcpp.F90)

      foreach(TESTFILE ${TEST_LIST})
        get_filename_component(TESTNAME ${TESTFILE} NAME_WE)
        add_executable(${TESTNAME}
                       ${CMAKE_SOURCE_DIR}/testing/fortran_tests/${TESTFILE})
        add_dependencies(${TESTNAME} ezproj ezprojf)
        target_include_directories(
          ${TESTNAME} PRIVATE ${CMAKE_SOURCE_DIR}/src
                              ${CMAKE_BINARY_DIR}/CMakeFiles/mod/ezprojf)
        target_link_libraries(${TESTNAME} ezproj ezprojf)
        set_target_properties(
          ${TESTNAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY
                                 ${CMAKE_BINARY_DIR}/fortran_tests)

        add_test(
          NAME TEST_${TESTNAME}
          COMMAND ${CMAKE_BINARY_DIR}/fortran_tests/${TESTNAME}
          WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/testing)
        if(CYGWIN)
          set_tests_properties(
            TEST_${TESTNAME} PROPERTIES ENVIRONMENT
                                        "PATH=$ENV{PATH}:${CMAKE_BINARY_DIR}")
        else(CYGWIN)
          set_tests_properties(
            TEST_${TESTNAME}
            PROPERTIES
              ENVIRONMENT
              "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_BINARY_DIR}")
        endif(CYGWIN)
      endforeach()
    endif(EZPROJ_BUILD_FORTRAN)
  endif(EZPROJ_BUILD_TESTS)

endif(UNIX OR CYGWIN)
# ##############################################################################
