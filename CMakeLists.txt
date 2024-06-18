cmake_minimum_required(VERSION 3.10)
include(ExternalProject)
include(FetchContent)

if (DEFINED CIMEROOT)
  message("Using CIME in ${CIMEROOT} with compiler ${COMPILER}")
  include(${CASEROOT}/Macros.cmake)
  if (${PIO_VERSION} LESS 2)
    message( FATAL_ERROR "Version 2 of the PIO library required")
  endif()
  if (MPILIB STREQUAL mpi-serial)
    set(CMAKE_Fortran_COMPILER ${SFC})
    set(CMAKE_C_COMPILER ${SCC})
  else()
    set(CMAKE_Fortran_COMPILER ${MPIFC})
    set(CMAKE_C_COMPILER ${MPICC})
  endif()
  set(CMAKE_Fortran_FLAGS "${FFLAGS} ${CPPDEFS} -I${LIBROOT}/include -I${LIBROOT}/nuopc/esmf/${NINST_VALUE}/include")
  add_compile_definitions(CESMCOUPLED)
  list(APPEND CMAKE_MODULE_PATH ${SRC_ROOT}/cime/CIME/non_py/src/CMake)
else()
  set(BLD_STANDALONE TRUE)
  project(SHARE LANGUAGES Fortran C VERSION 0.1)
  list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)
endif()
enable_language(Fortran)

option(WERROR "add the -Werror flag to compiler (works with gcc and intel)" OFF)

if (DEFINED ENV{PIO_ROOT})
  message("PIO_ROOT is $ENV{PIO_ROOT}")
else()
  if (DEFINED PIO)
    set(PIO_PATH ${PIO})
  else()
    set(PIO_PATH $ENV{PIO})
  endif()
  find_package(PIO REQUIRED COMPONENT C Fortran PATH ${PIO_PATH})
endif()

if (NOT DEFINED MPILIB OR NOT ${MPILIB} STREQUAL "mpi-serial")
  find_package(MPI REQUIRED)
endif()

if (DEFINED ENV{ESMF_ROOT})
  list(APPEND CMAKE_MODULE_PATH $ENV{ESMF_ROOT}/cmake)
endif()
message("ESMF cmake is ${CMAKE_MODULE_PATH}")
find_package(ESMF REQUIRED) 
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${ESMF_F90COMPILEPATHS}")



if("${COMPILER}" STREQUAL "nag")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -D__NAG__")
endif()
set(GENF90SOURCES src/shr_assert_mod.F90 src/shr_frz_mod.F90 src/shr_infnan_mod.F90)
#file(GLOB GENF90SOURCES "src/*.F90.in")
set(ENABLE_GENF90 ON)

#===== genf90 =====
if (DEFINED GENF90_PATH)
  add_custom_target(genf90
    DEPENDS ${GENF90_PATH}/genf90.pl)
else ()
  ExternalProject_Add (genf90
    PREFIX ${CMAKE_CURRENT_BINARY_DIR}/genf90
    GIT_REPOSITORY https://github.com/PARALLELIO/genf90
    GIT_TAG update_cmake_interface  
    UPDATE_COMMAND ""
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")
  ExternalProject_Get_Property (genf90 SOURCE_DIR)
  set (GENF90_PATH ${SOURCE_DIR})
  unset (SOURCE_DIR)
endif ()


set(GENF90 "${GENF90_PATH}/genf90.pl")
#include(${GENF90_PATH}/cmake/genf90_utils.cmake)

#===== Fortran Source Generation with GenF90 =====
foreach (SRC_FILE IN LISTS GENF90SOURCES)
  list(APPEND SHAREGENF90SRC ${SRC_FILE})
  add_custom_command (OUTPUT ${SRC_FILE}
    COMMAND ${GENF90_PATH}/genf90.pl
    ${CMAKE_CURRENT_SOURCE_DIR}/${SRC_FILE}.in > ${SRC_FILE}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${SRC_FILE}.in genf90)
endforeach ()

#process_genf90_source_list("${GENF90SOURCES}" ${CMAKE_CURRENT_BINARY_DIR} SHAREGENF90SRC)

file(GLOB SOURCES "src/*.c" "src/*.F90" "src/water_isotopes/*.F90" "RandNum/src/*.F90" "RandNum/src/*/*.F90" "RandNum/src/*/*.c")
#list(APPEND SOURCES "${SHAREGENF90SRC}")
#add_definitions(-DCPRINTEL)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/src)
add_library(share STATIC ${SOURCES} ${SHAREGENF90SRC})
add_dependencies (share genf90)
target_include_directories(share PRIVATE include RandNum/include)
#target_include_directories(share PRIVATE RandNum/include)


