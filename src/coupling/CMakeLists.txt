# See ../CMakeLists.txt for an explanation of the variables set here

file(GLOB coupling_lib_sources ${CMAKE_CURRENT_SOURCE_DIR}/*.F90)
list(APPEND share_lib_sources ${coupling_lib_sources})
sourcelist_to_parent(share_lib_sources)

list(APPEND share_sources
  shr_wtracers_mod.F90)
sourcelist_to_parent(share_sources)
