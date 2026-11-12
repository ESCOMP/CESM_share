# Local pFUnit files.
set(pf_sources
  test_glc_elevclass.pf)

# Sources to test.
set(sources_needed
  glc_elevclass_mod.F90
  shr_kind_mod.F90
  shr_sys_mod.nompi_abortthrows.F90
  shr_abort_mod.abortthrows.F90
  shr_assert_mod.F90
  shr_infnan_mod.F90
  shr_log_mod.F90
  shr_strconvert_mod.F90)

extract_sources("${sources_needed}" "${share_sources}" test_sources)

# Do source preprocessing and add the executable.
add_pfunit_ctest(glc_elevclass
  TEST_SOURCES "${pf_sources}"
  OTHER_SOURCES "${test_sources}")

declare_generated_dependencies(glc_elevclass "${share_genf90_sources}")

target_link_libraries(glc_elevclass esmf)
# The following adds all dependencies of ESMF, including PIO, NetCDF, etc.:
target_link_libraries(glc_elevclass ESMF::ESMF)
