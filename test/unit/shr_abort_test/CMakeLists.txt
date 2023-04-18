# Local pFUnit files.
set(pf_sources
  test_shr_abort.pf)

# Sources to test.
set(sources_needed
  shr_kind_mod.F90
  shr_abort_mod.abortthrows.F90)
extract_sources("${sources_needed}" "${share_sources}" test_sources)

# Do source preprocessing and add the executable.
add_pfunit_ctest(shr_abort_mod
  TEST_SOURCES "${pf_sources}"
  OTHER_SOURCES "${test_sources}")
