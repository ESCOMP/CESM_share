# See ../CMakeLists.txt for an explanation of the variables set here

file(GLOB util_lib_sources ${CMAKE_CURRENT_SOURCE_DIR}/*.F90 ${CMAKE_CURRENT_SOURCE_DIR}/*.c)
list(APPEND share_lib_sources ${util_lib_sources})
sourcelist_to_parent(share_lib_sources)

set(genf90_files shr_assert_mod.F90.in shr_infnan_mod.F90.in)
process_genf90_source_list("${genf90_files}" ${CMAKE_CURRENT_BINARY_DIR}
  util_genf90_sources)
list(APPEND share_genf90_sources ${util_genf90_sources})
sourcelist_to_parent(share_genf90_sources)

list(APPEND share_sources ${util_genf90_sources})
list(APPEND share_sources
  shr_file_mod.F90
  shr_sys_mod.F90
  shr_log_mod.F90
  shr_spfn_mod.F90
  shr_strconvert_mod.F90
  shr_cal_mod.F90
  shr_expr_parser_mod.F90
  shr_nl_mod.F90
  shr_string_mod.F90
  shr_timer_mod.F90
  shr_vmath_mod.F90
  shr_mpi_mod.F90
  shr_pio_mod.F90
  m_MergeSorts.F90
  nuopc_shr_methods.F90)
sourcelist_to_parent(share_sources)
