set (pf_sources
  test_shr_wiso.pf
  )

set(sources_needed
   shr_wiso_mod.F90
   shr_kind_mod.F90
   shr_const_mod.F90
   shr_sys_mod.nompi_abortthrows.F90
   shr_abort_mod.abortthrows.F90
   )

extract_sources("${sources_needed}" "${share_sources}" test_sources)

add_pfunit_ctest(shr_wiso
  TEST_SOURCES "${pf_sources}"
  OTHER_SOURCES "${test_sources}")

declare_generated_dependencies(shr_wiso "${share_genf90_sources}")
