# Local pFUnit files.
set(pf_sources
  test_wv_sat.pf test_wv_sat_each_method.pf)

# Sources to test.
set(sources_needed
  shr_kind_mod.F90 shr_const_mod.F90 shr_wv_sat_mod.F90)
extract_sources("${sources_needed}" "${share_sources}" test_sources)

# Do source preprocessing and add the executable.
add_pfunit_ctest(shr_wv_sat_mod
  TEST_SOURCES "${pf_sources}"
  OTHER_SOURCES "${test_sources}")
