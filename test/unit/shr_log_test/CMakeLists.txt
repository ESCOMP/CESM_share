set(pf_sources
  test_error_printers.pf)

set(sources_needed shr_kind_mod.F90 shr_infnan_mod.F90
  shr_strconvert_mod.F90 shr_log_mod.F90)

extract_sources("${sources_needed}" "${share_sources}" test_sources)

add_pfunit_ctest(shr_log
  TEST_SOURCES "${pf_sources}"
  OTHER_SOURCES "${test_sources}")

declare_generated_dependencies(shr_log "${share_genf90_sources}")

target_link_libraries(shr_log esmf)
# The following adds all dependencies of ESMF, including PIO, NetCDF, etc.:
target_link_libraries(shr_log ESMF::ESMF)
