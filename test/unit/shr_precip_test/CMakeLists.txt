# Local pFUnit files.
set(pf_sources
  test_shr_precip.pf)

# Sources to test.
set(sources_needed
  shr_kind_mod.F90 shr_const_mod.F90 shr_precip_mod.F90)
extract_sources("${sources_needed}" "${share_sources}" test_sources)

# Do source preprocessing and add the executable.
add_pfunit_ctest(shr_precip_mod
  TEST_SOURCES "${pf_sources}"
  OTHER_SOURCES "${test_sources}")
