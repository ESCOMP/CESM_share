# See ../CMakeLists.txt for an explanation of the variables set here

file(GLOB const_lib_sources ${CMAKE_CURRENT_SOURCE_DIR}/*.F90)
list(APPEND share_lib_sources ${const_lib_sources})
sourcelist_to_parent(share_lib_sources)

list(APPEND share_sources
  shr_kind_mod.F90
  shr_const_mod.F90
  shr_water_species_mod.F90)
sourcelist_to_parent(share_sources)
