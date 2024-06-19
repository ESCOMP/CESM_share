cmake_minimum_required(VERSION 3.26)
project(share)
include(ExternalProject)
set(genf90_files shr_infnan_mod.F90.in shr_assert_mod.F90.in)

#===== genf90 =====
if (DEFINED GENF90_PATH)
  add_custom_target(genf90
    DEPENDS ${GENF90_PATH}/genf90.pl)
else ()
  ExternalProject_Add (genf90
    PREFIX ${CMAKE_CURRENT_BINARY_DIR}/genf90
    GIT_REPOSITORY https://github.com/PARALLELIO/genf90
    GIT_TAG origin/update_cmake_interface
    UPDATE_COMMAND git pull "https://github.com/PARALLELIO/genf90"
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")
  ExternalProject_Get_Property (genf90 SOURCE_DIR)
  set (GENF90_PATH ${SOURCE_DIR})
  unset (SOURCE_DIR)
endif ()
include(${GENF90_PATH}/CMake/genf90_utils.cmake)

process_genf90_source_list("${genf90_files}" ${CMAKE_CURRENT_BINARY_DIR}
  share_genf90_sources)

#sourcelist_to_parent(share_genf90_sources)

list(APPEND share_sources "${share_genf90_sources}")

list(APPEND share_sources
  shr_file_mod.F90
  shr_kind_mod.F90
  shr_const_mod.F90
  shr_sys_mod.F90
  shr_log_mod.F90
  shr_orb_mod.F90
  shr_spfn_mod.F90
  shr_strconvert_mod.F90
  shr_cal_mod.F90
  shr_nl_mod.F90
  shr_precip_mod.F90
  shr_string_mod.F90
  shr_timer_mod.F90
  shr_vmath_mod.F90
  shr_wv_sat_mod.F90)

# Build a separate list containing the mct wrapper and its dependencies. That
# way, this list can be easily included in unit test builds that link to mct,
# but excluded from builds that do not include mct.
list(APPEND share_mct_sources
  mct_mod.F90
  shr_mct_mod.F90
  shr_mpi_mod.F90
  shr_pcdf_mod.F90)

# Build a separate list containing the pio wrapper and its dependencies. That
# way, this list can be easily included in unit test builds that include PIO or
# a stub of PIO, but excluded from builds that do not include PIO.
list(APPEND share_pio_sources
  shr_pio_mod.F90)

#sourcelist_to_parent(share_sources)
#sourcelist_to_parent(share_mct_sources)
#sourcelist_to_parent(share_pio_sources)
add_library(share ${share_sources})
add_dependencies (share genf90)
