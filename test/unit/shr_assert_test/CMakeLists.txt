set(pf_sources test_assert.pf test_assert_array.pf test_macro.pf
  test_ndebug.pf)

set(sources_needed shr_kind_mod.F90 shr_infnan_mod.F90
  shr_strconvert_mod.F90 shr_log_mod.F90
  shr_sys_mod.nompi_abortthrows.F90 shr_abort_mod.abortthrows.F90
  shr_assert_mod.F90)

extract_sources("${sources_needed}" "${share_sources}" test_sources)

add_pfunit_ctest(assert
  TEST_SOURCES "${pf_sources}"
  OTHER_SOURCES "${test_sources}")

declare_generated_dependencies(assert "${share_genf90_sources}")
