set (pf_sources
  test_shr_wtracers.pf
  )

set(sources_needed
  shr_wtracers_mod.F90
  shr_assert_mod.F90
  shr_infnan_mod.F90
  shr_kind_mod.F90
  shr_log_mod.F90
  shr_strconvert_mod.F90
  shr_string_mod.F90
  shr_sys_mod.nompi_abortthrows.F90
  shr_abort_mod.abortthrows.F90
  shr_timer_mod.F90
  nuopc_shr_methods.F90
  gptl.F90)

extract_sources("${sources_needed}" "${share_sources}" test_sources)

add_pfunit_ctest(shr_wtracers
  TEST_SOURCES "${pf_sources}"
  OTHER_SOURCES "${test_sources}")

declare_generated_dependencies(shr_wtracers "${share_genf90_sources}")

target_link_libraries(shr_wtracers esmf)
# The following adds all dependencies of ESMF, including PIO, NetCDF, etc.:
target_link_libraries(shr_wtracers ESMF::ESMF)
