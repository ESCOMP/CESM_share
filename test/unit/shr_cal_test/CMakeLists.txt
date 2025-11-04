# Local pFUnit files.
set(pf_sources
  test_shr_cal.pf)

# Sources to test.
set(sources_needed
  shr_cal_mod.F90
  shr_kind_mod.F90
  shr_const_mod.F90
  shr_sys_mod.nompi_abortthrows.F90
  shr_abort_mod.abortthrows.F90
  shr_string_mod.F90
  shr_strconvert_mod.F90
  shr_infnan_mod.F90
  shr_log_mod.F90
  shr_assert_mod.F90
  shr_timer_mod.F90)

extract_sources("${sources_needed}" "${share_sources}" test_sources)

# This is needed to make ESMF_Stubs.F90 avoid pulling in mpi
add_definitions(-DHIDE_MPI)

# Do source preprocessing and add the executable.
add_pfunit_ctest(shr_cal_mod
  TEST_SOURCES "${pf_sources}"
  OTHER_SOURCES "${test_sources}")

declare_generated_dependencies(shr_cal_mod "${share_genf90_sources}")
#set_target_properties(shr_cal_mod PROPERTIES LINK_FLAGS "${ESMF_F90ESMFLINKLIBS}")

target_link_libraries(shr_cal_mod esmf pioc netcdff netcdf)
# The following adds all dependencies of ESMF, including PIO, NetCDF, etc.:
target_link_libraries(shr_cal_mod ESMF::ESMF)
