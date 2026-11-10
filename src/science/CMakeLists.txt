# See ../CMakeLists.txt for an explanation of the variables set here

file(GLOB science_lib_sources ${CMAKE_CURRENT_SOURCE_DIR}/*.F90)
list(APPEND share_lib_sources ${science_lib_sources})
sourcelist_to_parent(share_lib_sources)

set(genf90_files shr_frz_mod.F90.in)
process_genf90_source_list("${genf90_files}" ${CMAKE_CURRENT_BINARY_DIR}
  science_genf90_sources)
list(APPEND share_genf90_sources ${science_genf90_sources})
sourcelist_to_parent(share_genf90_sources)

list(APPEND share_sources ${science_genf90_sources})
list(APPEND share_sources
  shr_orb_mod.F90
  shr_precip_mod.F90
  shr_wv_sat_mod.F90
  shr_wiso_mod.F90)
sourcelist_to_parent(share_sources)
