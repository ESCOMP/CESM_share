set(pf_sources
  test_toString.pf)

set(sources_needed shr_kind_mod.F90 shr_infnan_mod.F90
  shr_strconvert_mod.F90)

extract_sources("${sources_needed}" "${share_sources}" test_sources)

add_pfunit_ctest(shr_strconvert
  TEST_SOURCES "${pf_sources}"
  OTHER_SOURCES "${test_sources}")

declare_generated_dependencies(shr_strconvert "${share_genf90_sources}")
