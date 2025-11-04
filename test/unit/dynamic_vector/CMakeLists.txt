include_directories(.)

# Because these tests use type parameterization, unfortunately we need to
# preprocess *before* running the pFUnit preprocessor, then again *after*.
if(${CMAKE_C_COMPILER_ID} STREQUAL GNU OR ${CMAKE_C_COMPILER_ID} STREQUAL Clang OR ${CMAKE_C_COMPILER_ID} STREQUAL AppleClang)
  function(make_cpp_command varname start_file end_file)
    set(${varname} ${CMAKE_C_COMPILER} -E -x c ${start_file} -o ${end_file}
      PARENT_SCOPE)
  endfunction()
elseif(${CMAKE_C_COMPILER_ID} STREQUAL Intel OR ${CMAKE_C_COMPILER_ID} STREQUAL IntelLLVM)
  function(make_cpp_command varname start_file end_file)
    set(${varname} "${CMAKE_C_COMPILER} -E ${start_file} -o ${end_file}"
      PARENT_SCOPE)
  endfunction()
elseif(${CMAKE_C_COMPILER_ID} STREQUAL XL)
  function(make_cpp_command varname start_file end_file)
    get_filename_component(start_base ${start_file} NAME)
    string(REGEX REPLACE "\\.[^.]+$" ".i" cpp_output ${start_base})
    # Unfortunately, the C preprocessor doesn't like Fortran syntax, and
    # returns a non-zero error code even though it succeeds. Use "|| :" to
    # tell CMake that the command succeeded.
    set(${varname} ${CMAKE_C_COMPILER} -E ${start_file} > ${end_file} || :
      PARENT_SCOPE)
  endfunction()
endif()

# Function to preprocess the input to the output with the C preprocessor.
# Any extra arguments are interpreted as header files that preprocessing
# depends on. Unfortunately, we lose CMake's intrinsic capability to
# track included files as dependencies.
function(c_preprocess in_file out_file)
  if(IS_ABSOLUTE "${in_file}")
    set(start_file "${in_file}")
  else()
    set(start_file "${CMAKE_CURRENT_SOURCE_DIR}/${in_file}")
  endif()
  if(IS_ABSOLUTE "${out_file}")
    set(end_file "${out_file}")
  else()
    set(end_file "${CMAKE_CURRENT_BINARY_DIR}/${out_file}")
  endif()
  set(includes ${ARGN})

  make_cpp_command(cpp_command ${start_file} ${end_file})
  separate_arguments(cpp_command)
  add_custom_command(
    OUTPUT ${end_file}
    COMMAND ${cpp_command}
    DEPENDS ${start_file} ${includes}
    )

endfunction()

# Included in tests we preprocess here.
set(test_include
  ${CMAKE_CURRENT_SOURCE_DIR}/dynamic_vector_base_tests.inc)

# Clear before loop below.
unset(pf_sources)

# File with int_ptr type.
set(test_sources ptr_wrapper.F90)

set(sources_needed shr_kind_mod.F90 shr_infnan_mod.F90
  shr_strconvert_mod.F90 shr_log_mod.F90)
extract_sources("${sources_needed}" "${share_sources}" test_sources)

# Loop over type tests.
#
# The idea is that integer tests an intrinsic type, real(r8) is a type with
# a kind, character is special because of its length and substring syntax,
# and int_ptr is a simple derived type.

# "character16" could be in the following list, but right now it is broken
# on multiple compilers due to compiler bugs.
foreach(type IN ITEMS integer r8 int_ptr)
c_preprocess(${type}_vector_tests.pf.in ${type}_vector_tests.pf
  ${test_include})
  list(APPEND pf_sources
    ${CMAKE_CURRENT_BINARY_DIR}/${type}_vector_tests.pf)
  list(APPEND test_sources
    dynamic_vector_${type}.F90)
endforeach()

add_pfunit_ctest(dynamic_vector
  TEST_SOURCES "${pf_sources}"
  OTHER_SOURCES "${test_sources}")

declare_generated_dependencies(dynamic_vector "${share_genf90_sources}")

target_link_libraries(dynamic_vector esmf)
# The following adds all dependencies of ESMF, including PIO, NetCDF, etc.:
target_link_libraries(dynamic_vector ESMF::ESMF)
