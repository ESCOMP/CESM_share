set (pf_sources
  test_shr_string.pf
  )

set(sources_needed
  shr_string_mod.F90
  shr_assert_mod.F90
  shr_infnan_mod.F90
  shr_kind_mod.F90
  shr_log_mod.F90
  shr_strconvert_mod.F90
  shr_sys_mod.nompi_abortthrows.F90
  shr_abort_mod.abortthrows.F90
  shr_timer_mod.F90)

extract_sources("${sources_needed}" "${share_sources}" test_sources)

add_pfunit_ctest(shr_string
  TEST_SOURCES "${pf_sources}"
  OTHER_SOURCES "${test_sources}")

declare_generated_dependencies(shr_string "${share_genf90_sources}")

target_link_libraries(shr_string esmf)
