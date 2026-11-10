module shr_wtracers_mod

   !---------------------------------------------------------------------
   !
   ! Purpose:
   !
   ! This module contains data and routines for working with water tracers / isotopes.
   !
   ! Note that the init routine uses ESMF-style error handling (where a return code is
   ! returned up the call stack), whereas other routines abort directly if an error is
   ! detected. (The rationale for this is that the init routine has a lot of interaction
   ! with ESMF and is expected to be called from an ESMF-centric part of the code, whereas
   ! the other routines are less ESMF-centric in both their implementation and where they
   ! are expected to be called from, and so it doesn't make as much sense for them to
   ! follow the ESMF error handling paradigm).
   !---------------------------------------------------------------------

   use shr_kind_mod      , only : r8=>SHR_KIND_R8
   use shr_kind_mod      , only : CS=>SHR_KIND_CS, CM=>SHR_KIND_CM, CL=>SHR_KIND_CL, CX=>SHR_KIND_CX, CXX=>SHR_KIND_CXX
   use shr_log_mod       , only : shr_log_error
   use shr_log_mod       , only : s_logunit=>shr_log_Unit
   use shr_log_mod       , only : s_loglev=>shr_log_Level
   use shr_string_mod    , only : shr_string_listGetAllNames, shr_string_toUpper
   use shr_string_mod    , only : shr_string_withoutSuffix
   use shr_infnan_mod    , only : shr_infnan_isnan
   use shr_sys_mod       , only : shr_sys_abort
   use shr_water_species_mod, only : WATER_SPECIES_TYPE_BULK, WATER_SPECIES_TYPE_H218O
   use shr_water_species_mod, only : WATER_SPECIES_TYPE_H217O, WATER_SPECIES_TYPE_HDO
   use nuopc_shr_methods , only : chkerr
   use NUOPC             , only : NUOPC_CompAttributeGet
   use ESMF              , only : ESMF_GridComp
   use ESMF              , only : ESMF_SUCCESS

   implicit none
   private

   !--------------------------------------------------------------------------
   ! Public interfaces
   !--------------------------------------------------------------------------

   public :: shr_wtracers_init              ! initialize water tracer information
   public :: shr_wtracers_init_directly_for_testing ! initialize water tracer information directly for the sake of unit testing
   public :: shr_wtracers_finalize          ! finalize water tracer information
   public :: shr_wtracers_initialized       ! return true if this module has been initialized
   public :: shr_wtracers_is_wtracer_field  ! return true if the given field name is a water tracer field
   public :: shr_wtracers_get_bulk_fieldname ! return the name of the equivalent bulk field corresponding to a water tracer field
   public :: shr_wtracers_present           ! return true if there are water tracers in this simulation
   public :: shr_wtracers_get_num_tracers   ! get number of water tracers in this simulation
   public :: shr_wtracers_get_name          ! get the name of a given tracer
   public :: shr_wtracers_get_species_type  ! get the species type value associated with a given tracer
   public :: shr_wtracers_get_species_name  ! get the species name associated with a given tracer
   public :: shr_wtracers_is_isotope        ! return true if a given tracer is an isotope
   public :: shr_wtracers_get_initial_ratio ! get the initial ratio for a given tracer
   public :: shr_wtracers_check_tracer_ratios ! check tracer ratios against expectations

   interface shr_wtracers_check_tracer_ratios
      module procedure shr_wtracers_check_tracer_ratios_1d
      module procedure shr_wtracers_check_tracer_ratios_2d
   end interface shr_wtracers_check_tracer_ratios

   !--------------------------------------------------------------------------
   ! Private interfaces
   !--------------------------------------------------------------------------

   private :: shr_wtracers_parse_attributes   ! Parse water tracer NUOPC attributes
   private :: shr_wtracers_set_species_types  ! Set species types from species names
   private :: shr_wtracers_set_initial_ratios ! Set real-valued initial ratios from strings
   private :: shr_wtracers_print              ! Print tracer info to log
   private :: shr_wtracers_check_tracer_num   ! Check a tracer_num argument and abort if invalid
   private :: shr_wtracers_should_skip_check  ! Return true if a variable should skip tracer-ratio checks

   !--------------------------------------------------------------------------
   ! Public data
   !--------------------------------------------------------------------------

   ! Max length of various strings
   integer, parameter, public :: WTRACER_NAME_MAXLEN = CM
   integer, parameter, public :: WTRACER_SPECIES_NAME_MAXLEN = CM

   ! Species name associated with bulk water tracers
   character(len=*), parameter, public :: WATER_SPECIES_NAME_BULK = "-"

   ! Suffix for water tracer field names
   character(len=*), parameter, public :: WTRACERS_SUFFIX = "_wtracers"

   !--------------------------------------------------------------------------
   ! Private data
   !--------------------------------------------------------------------------

   integer :: num_tracers
   character(len=WTRACER_NAME_MAXLEN), allocatable :: tracer_names(:)
   integer, allocatable :: tracer_species_types(:)
   character(len=WTRACER_SPECIES_NAME_MAXLEN), allocatable :: tracer_species_names(:)
   real(r8), allocatable :: tracer_initial_ratios(:)
   character(len=CL), allocatable :: variables_not_checked(:)
   logical :: water_tracers_initialized = .false.

   character(len=*), parameter :: u_FILE_u = &
        __FILE__

contains

   !-----------------------------------------------------------------------
   subroutine shr_wtracers_init(driver, maintask, rc)
      !
      ! !DESCRIPTION:
      ! Initialize water tracer information
      !
      ! If there are any errors, an ESMF error code is returned in rc
      !
      ! !ARGUMENTS
      type(ESMF_GridComp), intent(in) :: driver
      logical, intent(in)  :: maintask  ! true if this is the main task (for i/o)
      integer, intent(out) :: rc
      !
      ! !LOCAL VARIABLES
      character(len=*), parameter :: subname='shr_wtracers_init'
      !---------------------------------------------------------------

      rc = ESMF_SUCCESS

      if (water_tracers_initialized) then
         call shr_log_error("Attempt to call "//subname//" multiple times", rc=rc)
         return
      end if

      call shr_wtracers_parse_attributes(driver, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return

      water_tracers_initialized = .true.

      if (s_loglev > 0) then
         call shr_wtracers_print(maintask, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
      end if

   end subroutine shr_wtracers_init

   !-----------------------------------------------------------------------
   subroutine shr_wtracers_init_directly_for_testing( &
              water_tracer_names, water_tracer_species, water_tracer_initial_ratios, &
              variables_not_checked_in, rc)
      !
      ! !DESCRIPTION:
      ! Initialize water tracer information directly for the sake of unit testing
      !
      ! If there are any errors, an ESMF error code is returned in rc
      !
      ! !ARGUMENTS
      character(len=*), intent(in) :: water_tracer_names(:)
      character(len=*), intent(in) :: water_tracer_species(:)  ! expected to be uppercase
      real(r8), intent(in) :: water_tracer_initial_ratios(:)
      character(len=*), intent(in), optional :: variables_not_checked_in(:)
      integer, intent(out) :: rc
      !
      ! !LOCAL VARIABLES
      character(len=*), parameter :: subname='shr_wtracers_init_directly_for_testing'
      !---------------------------------------------------------------

      rc = ESMF_SUCCESS

      if (water_tracers_initialized) then
         call shr_log_error("Attempt to call "//subname//" multiple times", rc=rc)
         return
      end if

      num_tracers = size(water_tracer_names)
      if (size(water_tracer_species) /= num_tracers .or. &
          size(water_tracer_initial_ratios) /= num_tracers) then
         call shr_log_error(subname//": Input array sizes disagree", rc=rc)
         return
      end if

      tracer_names = water_tracer_names
      tracer_species_names = water_tracer_species
      call shr_wtracers_set_species_types(rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      tracer_initial_ratios = water_tracer_initial_ratios

      if (present(variables_not_checked_in)) then
         variables_not_checked = variables_not_checked_in
      else
         ! No variables_not_checked for this run; deallocate to clear out any setting from
         ! previous tests.
         if (allocated(variables_not_checked)) deallocate(variables_not_checked)
      end if

      water_tracers_initialized = .true.

   end subroutine shr_wtracers_init_directly_for_testing

   !-----------------------------------------------------------------------
   function shr_wtracers_initialized()
      !
      ! !DESCRIPTION:
      ! Return true if this module has been initialized
      !
      ! !ARGUMENTS
      logical :: shr_wtracers_initialized  ! function result
      !
      ! !LOCAL VARIABLES:
      character(len=*), parameter :: subname='shr_wtracers_initialized'
      !-----------------------------------------------------------------------

      shr_wtracers_initialized = water_tracers_initialized

   end function shr_wtracers_initialized

   !-----------------------------------------------------------------------
   subroutine shr_wtracers_parse_attributes(driver, rc)
      !
      ! !DESCRIPTION:
      ! Parse water tracer NUOPC attributes
      !
      ! This parses four attributes, which are all colon-delimited strings, and which all
      ! except water_tracers_variables_not_checked must have the same number of elements
      ! (this requirement is checked here):
      ! - water_tracer_names (arbitrary user-defined names)
      ! - water_tracer_species (corresponding to predetermined strings like "H218O", or
      !   the string given by WATER_SPECIES_NAME_BULK)
      ! - water_tracer_initial_ratios (strings that are convertable to real numbers)
      ! - water_tracers_variables_not_checked (tracer variable names for which checks should be skipped)
      !
      ! !ARGUMENTS
      type(ESMF_GridComp), intent(in) :: driver
      integer, intent(out) :: rc
      !
      ! !LOCAL VARIABLES
      integer :: localrc
      character(len=CXX) :: cvalue
      character(len=CXX) :: cvalue_upper
      logical :: isPresent, isSet
      character(len=CS), allocatable :: tracer_initial_ratios_str(:)
      character(len=*), parameter :: subname='shr_wtracers_parse_attributes'
      !---------------------------------------------------------------

      rc = ESMF_SUCCESS

      ! An empty string for the value of water_tracer_names leads to an error return code
      ! unless we check isPresent and/or isSet; these two logicals end up being false in
      ! this situation. To be safe, we check all of isPresent, isSet and len_trim(cvalue)
      ! == 0.
      call NUOPC_CompAttributeGet(driver, name="water_tracer_names", value=cvalue, &
           isPresent=isPresent, isSet=isSet, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      if (.not. isPresent .or. .not. isSet .or. len_trim(cvalue) == 0) then
         ! Avoid trying to process an empty list, because this is invalid in the
         ! shr_string_list routines
         num_tracers = 0
      else
         call shr_string_listGetAllNames(cvalue, tracer_names, rc=localrc)
         if (localrc /= 0) then
            call shr_log_error(subname//": error processing water_tracer_names", rc=rc)
            return
         end if
         num_tracers = size(tracer_names)
      end if

      call NUOPC_CompAttributeGet(driver, name="water_tracer_species", value=cvalue, &
           isPresent=isPresent, isSet=isSet, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      if (.not. isPresent .or. .not. isSet .or. len_trim(cvalue) == 0) then
         if (num_tracers > 0) then
            call shr_log_error(subname//": empty water_tracer_species despite non-empty water_tracer_names", rc=rc)
            return
         end if
      else
         cvalue_upper = shr_string_toUpper(cvalue)
         call shr_string_listGetAllNames(cvalue_upper, tracer_species_names, rc=localrc)
         if (localrc /= 0) then
            call shr_log_error(subname//": error processing water_tracer_species", rc=rc)
            return
         end if
         if (size(tracer_species_names) /= num_tracers) then
            call shr_log_error( &
                 subname//": different number of elements in water_tracer_names and water_tracer_species", &
                 rc=rc)
            return
         end if

         call shr_wtracers_set_species_types(rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
      end if

      call NUOPC_CompAttributeGet(driver, name="water_tracer_initial_ratios", value=cvalue, &
           isPresent=isPresent, isSet=isSet, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      if (.not. isPresent .or. .not. isSet .or. len_trim(cvalue) == 0) then
         if (num_tracers > 0) then
            call shr_log_error(subname//": empty water_tracer_initial_ratios despite non-empty water_tracer_names", rc=rc)
            return
         end if
      else
         call shr_string_listGetAllNames(cvalue, tracer_initial_ratios_str, rc=localrc)
         if (localrc /= 0) then
            call shr_log_error(subname//": error processing water_tracer_initial_ratios", rc=rc)
            return
         end if
         if (size(tracer_initial_ratios_str) /= num_tracers) then
            call shr_log_error( &
                 subname//": different number of elements in water_tracer_names and water_tracer_initial_ratios", &
                 rc=rc)
            return
         end if

         call shr_wtracers_set_initial_ratios(tracer_initial_ratios_str, rc=rc)
         if (chkerr(rc,__LINE__,u_FILE_u)) return
      end if

      call NUOPC_CompAttributeGet(driver, name="water_tracers_variables_not_checked", value=cvalue, &
           isPresent=isPresent, isSet=isSet, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
      if (.not. isPresent .or. .not. isSet .or. len_trim(cvalue) == 0) then
         ! For typical code paths, it should never be the case that variables_not_checked
         ! is already allocated. But check this and deallocate it to be safe in case there
         ! is some unusual code path where it was set at one point but is no longer set -
         ! in which case we want to clear out the old list.
         if (allocated(variables_not_checked)) deallocate(variables_not_checked)
      else
         call shr_string_listGetAllNames(cvalue, variables_not_checked, rc=localrc)
         if (localrc /= 0) then
            call shr_log_error(subname//": error processing water_tracers_variables_not_checked", rc=rc)
            return
         end if
      end if

   end subroutine shr_wtracers_parse_attributes

   !-----------------------------------------------------------------------
   subroutine shr_wtracers_set_species_types(rc)
      !
      ! !DESCRIPTION:
      ! Set species types from species names
      !
      ! !ARGUMENTS
      integer, intent(out) :: rc
      !
      ! !LOCAL VARIABLES
      integer :: i
      character(len=*), parameter :: subname='shr_wtracers_set_species_types'
      !---------------------------------------------------------------

      rc = ESMF_SUCCESS

      allocate(tracer_species_types(num_tracers))

      do i = 1, num_tracers
         select case (tracer_species_names(i))
         case (WATER_SPECIES_NAME_BULK)
            tracer_species_types(i) = WATER_SPECIES_TYPE_BULK
         case ("H218O")
            tracer_species_types(i) = WATER_SPECIES_TYPE_H218O
         case ("H217O")
            tracer_species_types(i) = WATER_SPECIES_TYPE_H217O
         case ("HDO")
            tracer_species_types(i) = WATER_SPECIES_TYPE_HDO
         case default
            call shr_log_error( &
                 subname//": unrecognized water species name '"//trim(tracer_species_names(i))//"'", &
                 rc=rc)
            return
         end select
      end do

   end subroutine shr_wtracers_set_species_types

   !-----------------------------------------------------------------------
   subroutine shr_wtracers_set_initial_ratios(tracer_initial_ratios_str, rc)
      !
      ! !DESCRIPTION:
      ! Set real-valued initial ratios from strings
      !
      ! !ARGUMENTS
      character(len=*), intent(in) :: tracer_initial_ratios_str(:)  ! string versions of initial ratios
      integer, intent(out) :: rc
      !
      ! !LOCAL VARIABLES
      integer :: i
      integer :: ios
      character(len=*), parameter :: subname='shr_wtracers_set_initial_ratios'
      !---------------------------------------------------------------

      rc = ESMF_SUCCESS

      allocate(tracer_initial_ratios(num_tracers))

      do i = 1, num_tracers
         read(tracer_initial_ratios_str(i), *, iostat=ios) tracer_initial_ratios(i)
         if (ios /= 0) then
            call shr_log_error( &
                 subname//": error reading initial ratio '"//trim(tracer_initial_ratios_str(i))//"'", &
                 rc=rc)
            return
         end if
      end do

   end subroutine shr_wtracers_set_initial_ratios

   !-----------------------------------------------------------------------
   subroutine shr_wtracers_print(maintask, rc)
      !
      ! !DESCRIPTION:
      ! Print tracer info to log
      !
      ! !ARGUMENTS
      logical, intent(in)  :: maintask  ! true if this is the main task
      integer, intent(out) :: rc
      !
      ! !LOCAL VARIABLES
      integer :: i
      character(len=*), parameter :: subname='shr_wtracers_print'
      !---------------------------------------------------------------

      rc = ESMF_SUCCESS

      ! The use of the various getters in the following code is partly for the sake of
      ! testing these getters to ensure they work right (via inspection of the output).
      if (maintask) then
         if (shr_wtracers_present()) then
            write(s_logunit, '(A)') "Water Tracers:"
         else
            write(s_logunit, '(A)') "No Water Tracers in this simulation"
         end if
         do i = 1, shr_wtracers_get_num_tracers()
            write(s_logunit, '(3X,A,I0,A)') "Tracer #", i, ":"
            write(s_logunit, '(6X,A,A)') "Name: ", trim(shr_wtracers_get_name(i))
            write(s_logunit, '(6X,A,A,A,I0,A)') "Species: ", trim(shr_wtracers_get_species_name(i)), &
                 " (", shr_wtracers_get_species_type(i), ")"
            write(s_logunit, '(6X,A,L1)') "Isotope? ", shr_wtracers_is_isotope(i)
            write(s_logunit, '(6X,A,G23.17)') "Initial ratio: ", shr_wtracers_get_initial_ratio(i)
         end do
      end if

   end subroutine shr_wtracers_print

   !-----------------------------------------------------------------------
   pure function shr_wtracers_should_skip_check(variable_name)
      !
      ! !DESCRIPTION:
      ! Return true if the given variable name should skip tracer-ratio checks
      !
      ! !ARGUMENTS
      character(len=*), intent(in) :: variable_name
      logical :: shr_wtracers_should_skip_check
      !
      ! !LOCAL VARIABLES
      integer :: i
      !-----------------------------------------------------------------------
      shr_wtracers_should_skip_check = .false.
      if (.not. allocated(variables_not_checked)) return
      do i = 1, size(variables_not_checked)
         if (variables_not_checked(i) == variable_name) then
            shr_wtracers_should_skip_check = .true.
            return
         end if
      end do
   end function shr_wtracers_should_skip_check

   !-----------------------------------------------------------------------
   subroutine shr_wtracers_check_tracer_num(tracer_num, subname)
      !
      ! !DESCRIPTION:
      ! Check a tracer_num argument and abort with a meaningful message if invalid
      !
      ! !ARGUMENTS
      integer, intent(in) :: tracer_num
      character(len=*), intent(in) :: subname  ! name of the caller, for error message
      !
      ! !LOCAL VARIABLES
      character(len=CX) :: msg
      !-----------------------------------------------------------------------
      if (tracer_num < 1 .or. tracer_num > num_tracers) then
         if (num_tracers == 0) then
            write(msg, '(A,I0,A)') &
                 subname//" ERROR: tracer_num out of range: ", tracer_num, &
                 " (This simulation has no tracers.)"
         else
            write(msg, '(A,I0,A,I0,A)') &
                 subname//" ERROR: tracer_num out of range: ", tracer_num, &
                 " (Valid range: 1 - ", num_tracers, ")"
         end if
         call shr_sys_abort(trim(msg))
      end if
   end subroutine shr_wtracers_check_tracer_num

   !-----------------------------------------------------------------------
   function shr_wtracers_is_wtracer_field(fieldname)
      !
      ! !DESCRIPTION:
      ! Return true if the given field name is a water tracer field
      !
      ! Note that, unlike most other routines in this module, this function works even if
      ! the data in this module has not been initialized (i.e., even if shr_wtracers_init
      ! has not been called): it works simply based on naming conventions.
      !
      ! !ARGUMENTS
      character(len=*), intent(in) :: fieldname
      logical :: shr_wtracers_is_wtracer_field  ! function result
      !
      ! !LOCAL VARIABLES:
      integer :: localrc
      logical :: is_tracer
      character(len=CX) :: msg

      character(len=*), parameter :: subname='shr_wtracers_is_wtracer_field'
      !-----------------------------------------------------------------------

      call shr_string_withoutSuffix( &
           in_str = fieldname, &
           suffix = WTRACERS_SUFFIX, &
           has_suffix = is_tracer, &
           rc = localrc)
      if (localrc /= 0) then
         write(msg, '(A,I0)') &
              subname//": ERROR in shr_string_withoutSuffix for fieldname '"// &
              trim(fieldname)//"', localrc = ", localrc
         call shr_sys_abort(trim(msg))
      end if
      shr_wtracers_is_wtracer_field = is_tracer
   end function shr_wtracers_is_wtracer_field

   !-----------------------------------------------------------------------
   subroutine shr_wtracers_get_bulk_fieldname(fieldname, is_wtracer_field, bulk_fieldname)
      !
      ! !DESCRIPTION:
      ! Return the name of the equivalent bulk field corresponding to a water tracer field
      !
      ! If fieldname is the name of a water tracer field, based on naming conventions,
      ! then is_wtracer_field will be true and bulk_fieldname will hold the name of the
      ! corresponding bulk field.
      !
      ! If fieldname is *not* the name of a water tracer field, then is_wtracer_field will
      ! be false, and bulk_fieldname will be the same as fieldname.
      !
      ! Note that, unlike most other routines in this module, this function works even if
      ! the data in this module has not been initialized (i.e., even if shr_wtracers_init
      ! has not been called): it works simply based on naming conventions.
      !
      ! !ARGUMENTS
      character(len=*), intent(in)  :: fieldname
      logical         , intent(out) :: is_wtracer_field
      character(len=*), intent(out) :: bulk_fieldname
      !
      ! !LOCAL VARIABLES:
      integer :: localrc
      character(len=CX) :: msg

      character(len=*), parameter :: subname='shr_wtracers_get_bulk_fieldname'
      !-----------------------------------------------------------------------

      call shr_string_withoutSuffix( &
           in_str = fieldname, &
           suffix = WTRACERS_SUFFIX, &
           has_suffix = is_wtracer_field, &
           out_str = bulk_fieldname, &
           rc = localrc)
      if (localrc /= 0) then
         write(msg, '(A,I0)') &
              subname//": ERROR in shr_string_withoutSuffix for fieldname '"// &
              trim(fieldname)//"', localrc = ", localrc
         call shr_sys_abort(trim(msg))
      end if
   end subroutine shr_wtracers_get_bulk_fieldname

   !-----------------------------------------------------------------------
   function shr_wtracers_present()
      !
      ! !DESCRIPTION:
      ! Return true if there are water tracers in this simulation
      !
      ! !ARGUMENTS
      logical :: shr_wtracers_present  ! function result
      !
      ! !LOCAL VARIABLES:
      character(len=*), parameter :: subname='shr_wtracers_present'
      !-----------------------------------------------------------------------
      if (.not. water_tracers_initialized) then
         call shr_sys_abort(subname//" ERROR: water tracers not yet initialized")
      end if

      shr_wtracers_present = (num_tracers > 0)
   end function shr_wtracers_present

   !-----------------------------------------------------------------------
   function shr_wtracers_get_num_tracers()
      !
      ! !DESCRIPTION:
      ! Get number of water tracers in this simulation
      !
      ! !ARGUMENTS
      integer :: shr_wtracers_get_num_tracers  ! function result
      !
      ! !LOCAL VARIABLES:
      character(len=*), parameter :: subname='shr_wtracers_get_num_tracers'
      !-----------------------------------------------------------------------
      if (.not. water_tracers_initialized) then
         call shr_sys_abort(subname//" ERROR: water tracers not yet initialized")
      end if

      shr_wtracers_get_num_tracers = num_tracers
   end function shr_wtracers_get_num_tracers

   !-----------------------------------------------------------------------
   function shr_wtracers_get_name(tracer_num)
      !
      ! !DESCRIPTION:
      ! Get the name of a given tracer
      !
      ! !ARGUMENTS
      integer, intent(in) :: tracer_num
      character(len=WTRACER_NAME_MAXLEN) :: shr_wtracers_get_name  ! function result
      !
      ! !LOCAL VARIABLES:
      character(len=*), parameter :: subname='shr_wtracers_get_name'
      !-----------------------------------------------------------------------
      if (.not. water_tracers_initialized) then
         call shr_sys_abort(subname//" ERROR: water tracers not yet initialized")
      end if
      call shr_wtracers_check_tracer_num(tracer_num, subname)

      shr_wtracers_get_name = tracer_names(tracer_num)
   end function shr_wtracers_get_name

   !-----------------------------------------------------------------------
   function shr_wtracers_get_species_type(tracer_num)
      !
      ! !DESCRIPTION:
      ! Get the species type value associated with a given tracer
      !
      ! !ARGUMENTS
      integer, intent(in) :: tracer_num
      integer :: shr_wtracers_get_species_type  ! function result
      !
      ! !LOCAL VARIABLES:
      character(len=*), parameter :: subname='shr_wtracers_get_species_type'
      !-----------------------------------------------------------------------
      if (.not. water_tracers_initialized) then
         call shr_sys_abort(subname//" ERROR: water tracers not yet initialized")
      end if
      call shr_wtracers_check_tracer_num(tracer_num, subname)

      shr_wtracers_get_species_type = tracer_species_types(tracer_num)
   end function shr_wtracers_get_species_type

   !-----------------------------------------------------------------------
   function shr_wtracers_get_species_name(tracer_num)
      !
      ! !DESCRIPTION:
      ! Get the species name associated with a given tracer
      !
      ! !ARGUMENTS
      integer, intent(in) :: tracer_num
      character(len=WTRACER_SPECIES_NAME_MAXLEN) :: shr_wtracers_get_species_name  ! function result
      !
      ! !LOCAL VARIABLES:
      character(len=*), parameter :: subname='shr_wtracers_get_species_name'
      !-----------------------------------------------------------------------
      if (.not. water_tracers_initialized) then
         call shr_sys_abort(subname//" ERROR: water tracers not yet initialized")
      end if
      call shr_wtracers_check_tracer_num(tracer_num, subname)

      shr_wtracers_get_species_name = tracer_species_names(tracer_num)
   end function shr_wtracers_get_species_name

   !-----------------------------------------------------------------------
   function shr_wtracers_is_isotope(tracer_num)
      !
      ! !DESCRIPTION:
      ! Return true if a given tracer is an isotope
      !
      ! !ARGUMENTS
      integer, intent(in) :: tracer_num
      logical :: shr_wtracers_is_isotope  ! function result
      !
      ! !LOCAL VARIABLES:
      character(len=*), parameter :: subname='shr_wtracers_is_isotope'
      !-----------------------------------------------------------------------
      if (.not. water_tracers_initialized) then
         call shr_sys_abort(subname//" ERROR: water tracers not yet initialized")
      end if
      call shr_wtracers_check_tracer_num(tracer_num, subname)

      shr_wtracers_is_isotope = (tracer_species_types(tracer_num) /= WATER_SPECIES_TYPE_BULK)
   end function shr_wtracers_is_isotope

   !-----------------------------------------------------------------------
   function shr_wtracers_get_initial_ratio(tracer_num)
      !
      ! !DESCRIPTION:
      ! Get the initial ratio for a given tracer
      !
      ! !ARGUMENTS
      integer, intent(in) :: tracer_num
      real(r8) :: shr_wtracers_get_initial_ratio  ! function result
      !
      ! !LOCAL VARIABLES:
      character(len=*), parameter :: subname='shr_wtracers_get_initial_ratio'
      !-----------------------------------------------------------------------
      if (.not. water_tracers_initialized) then
         call shr_sys_abort(subname//" ERROR: water tracers not yet initialized")
      end if
      call shr_wtracers_check_tracer_num(tracer_num, subname)

      shr_wtracers_get_initial_ratio = tracer_initial_ratios(tracer_num)
   end function shr_wtracers_get_initial_ratio

   !-----------------------------------------------------------------------
   subroutine shr_wtracers_check_tracer_ratios_1d(tracers, bulk, variable_name, check_skipped, extra_dim_index)
      !
      ! !DESCRIPTION:
      ! Check tracer ratios (tracer/bulk) against expectations
      !
      ! Aborts if any inconsistencies are found
      !
      ! Should only be called in simulations set up to maintain constant water tracer
      ! ratios: in general, water tracers will deviate from their initial, fixed ratios,
      ! and so it makes no sense to perform these checks since they will always fail.
      !
      ! If the given variable_name is in the list of variables to skip, the check is
      ! skipped and check_skipped is set to true.
      !
      ! !ARGUMENTS
      real(r8), intent(in) :: tracers(:,:)  ! dimensioned [tracerNum, gridcell]
      real(r8), intent(in) :: bulk(:)
      character(len=*), intent(in) :: variable_name  ! for diagnostic output; check is skipped if this name is in variables_not_checked
      logical, intent(out) :: check_skipped  ! true if the check was skipped (variable_name in variables_not_checked)
      integer, intent(in), optional :: extra_dim_index  ! index of extra dimension (for error messages)
      !
      ! !LOCAL VARIABLES
      integer :: n, i
      logical :: arrays_equal
      integer :: diff_tracer, diff_loc
      real(r8) :: val_bulk, val_tracer
      character(len=CX) :: msg

      real(r8), parameter :: tolerance = 1.0e-7_r8

      character(len=*), parameter :: subname='shr_wtracers_check_tracer_ratios_1d'
      ! In some error messages, it makes more sense to print the generic name:
      character(len=*), parameter :: subname_generic='shr_wtracers_check_tracer_ratios'
      !-----------------------------------------------------------------------
      check_skipped = .false.
      if (.not. water_tracers_initialized) then
         call shr_sys_abort(subname//" ERROR: water tracers not yet initialized")
      end if
      if (shr_wtracers_should_skip_check(variable_name)) then
         check_skipped = .true.
         return
      end if
      if (size(tracers, 1) /= num_tracers) then
         write(msg, '(A,I0,A,I0)') &
              subname//" ERROR: unexpected number of tracers: size(tracers, 1) = ", &
              size(tracers, 1), ", num_tracers = ", num_tracers
         call shr_sys_abort(trim(msg))
      end if
      if (size(tracers, 2) /= size(bulk)) then
         write(msg, '(A,I0,A,I0)') &
              subname//" ERROR: inconsistent sizes for tracers and bulk: size(tracers, 2) = ", &
              size(tracers, 2), ", size(bulk) = ", size(bulk)
         call shr_sys_abort(trim(msg))
      end if

      arrays_equal = .true.
      tracer_loop: do n = 1, num_tracers
         ! We may eventually want a mechanism to denote certain tracers as for-checking
         ! and others not-for-checking (probably via another nuopc attribute parallel to
         ! the other water tracers attributes). If we do, we could check that flag here
         ! for tracer #n and go on to the next tracer if this is a not-for-checking
         ! tracer.

         do i = 1, size(bulk)
            if (.not. shr_infnan_isnan(bulk(i)) .and. .not. shr_infnan_isnan(tracers(n,i))) then
               ! neither value is nan: check error tolerance
               val_bulk = bulk(i) * tracer_initial_ratios(n)
               val_tracer = tracers(n,i)
               if (abs(val_bulk - val_tracer) > tolerance * max(abs(val_bulk), abs(val_tracer))) then
                  arrays_equal = .false.
                  diff_tracer = n
                  diff_loc = i
                  exit tracer_loop
               end if
            else if (shr_infnan_isnan(bulk(i)) .and. shr_infnan_isnan(tracers(n,i))) then
               ! both values are nan: values are considered equal (do nothing)
            else
               ! only one value is nan: not equal
               arrays_equal = .false.
               diff_tracer = n
               diff_loc = i
               exit tracer_loop
            end if
         end do
      end do tracer_loop

      if (.not. arrays_equal) then
         write(s_logunit, '(A,A)') subname_generic, " ERROR: tracer does not agree with bulk water"
         write(s_logunit, '(A,A)') "Variable: ", trim(variable_name)
         if (present(extra_dim_index)) then
            write(s_logunit, '(A,I0)') "Extra dimension index: ", extra_dim_index
         end if
         write(s_logunit, '(A,I0,A,A)') "First difference found for tracer #", diff_tracer, &
              ": ", tracer_names(diff_tracer)
         write(s_logunit, '(A,I0)') "First difference at index: ", diff_loc
         write(s_logunit, '(A, ES25.17)') "Bulk  : ", bulk(diff_loc)
         write(s_logunit, '(A, ES25.17)') "Tracer: ", tracers(diff_tracer, diff_loc)
         write(s_logunit, '(A, ES25.17)') "Expected ratio: ", tracer_initial_ratios(diff_tracer)
         if (.not. shr_infnan_isnan(bulk(diff_loc))) then
            write(s_logunit, '(A, ES25.17)') "Bulk*ratio: ", bulk(diff_loc) * tracer_initial_ratios(diff_tracer)
         end if
         write(msg, '(A,I0)') &
              subname_generic//" ERROR: tracer does not agree with bulk water for variable '"// &
              trim(variable_name)//"', tracer '"//trim(tracer_names(diff_tracer))//"', at index ", diff_loc
         call shr_sys_abort(trim(msg))
      end if

   end subroutine shr_wtracers_check_tracer_ratios_1d

   !-----------------------------------------------------------------------
   subroutine shr_wtracers_check_tracer_ratios_2d(tracers, bulk, variable_name, check_skipped)
      !
      ! !DESCRIPTION:
      ! Check tracer ratios (tracer/bulk) against expectations for 2-d bulk arrays
      !
      ! This is a lightweight wrapper around shr_wtracers_check_tracer_ratios_1d that
      ! loops over the first dimension of the bulk array.
      !
      ! Aborts if any inconsistencies are found
      !
      ! Should only be called in simulations set up to maintain constant water tracer
      ! ratios: in general, water tracers will deviate from their initial, fixed ratios,
      ! and so it makes no sense to perform these checks since they will always fail.
      !
      ! If the given variable_name is in the list of variables to skip, the check is
      ! skipped and check_skipped is set to true.
      !
      ! !ARGUMENTS
      real(r8), intent(in) :: tracers(:,:,:)  ! dimensioned [ungriddedDim, tracerNum, gridcell]
      real(r8), intent(in) :: bulk(:,:)       ! dimensioned [ungriddedDim, gridcell]
      character(len=*), intent(in) :: variable_name    ! for diagnostic output; check is skipped if this name is in variables_not_checked
      logical, intent(out) :: check_skipped  ! true if the check was skipped (variable_name in variables_not_checked)
      !
      ! !LOCAL VARIABLES
      integer :: i
      logical :: check_skipped_1d
      character(len=CX) :: msg

      character(len=*), parameter :: subname='shr_wtracers_check_tracer_ratios_2d'
      !-----------------------------------------------------------------------
      check_skipped = .false.
      if (.not. water_tracers_initialized) then
         call shr_sys_abort(subname//" ERROR: water tracers not yet initialized")
      end if
      if (shr_wtracers_should_skip_check(variable_name)) then
         check_skipped = .true.
         return
      end if
      if (size(tracers, 1) /= size(bulk, 1)) then
         write(msg, '(A,I0,A,I0)') &
              subname//" ERROR: inconsistent size for tracers dim 1 and bulk dim 1: size(tracers, 1) = ", &
              size(tracers, 1), ", size(bulk, 1) = ", size(bulk, 1)
         call shr_sys_abort(trim(msg))
      end if
      if (size(tracers, 2) /= num_tracers) then
         write(msg, '(A,I0,A,I0)') &
              subname//" ERROR: unexpected number of tracers: size(tracers, 2) = ", &
              size(tracers, 2), ", num_tracers = ", num_tracers
         call shr_sys_abort(trim(msg))
      end if
      if (size(tracers, 3) /= size(bulk, 2)) then
         write(msg, '(A,I0,A,I0)') &
              subname//" ERROR: inconsistent size for tracers dim 3 and bulk dim 2: size(tracers, 3) = ", &
              size(tracers, 3), ", size(bulk, 2) = ", size(bulk, 2)
         call shr_sys_abort(trim(msg))
      end if

      do i = 1, size(bulk, 1)
         call shr_wtracers_check_tracer_ratios_1d(tracers(i,:,:), bulk(i,:), variable_name, &
              check_skipped=check_skipped_1d, extra_dim_index=i)
         ! We set the check_skipped output to true if the check was skipped for *any* 1d
         ! level. (In practice, this shouldn't happen unless we introduce new scenarios
         ! where a variable is skipped, so that the pre-check at the top of this 2d
         ! routine doesn't check all possible scenarios.)
         if (check_skipped_1d) check_skipped = .true.
      end do

   end subroutine shr_wtracers_check_tracer_ratios_2d

   !-----------------------------------------------------------------------
   subroutine shr_wtracers_finalize(rc)
      !
      ! !DESCRIPTION:
      ! Finalize (deallocate, clean up, etc.) water tracer information
      !
      ! !ARGUMENTS
      integer, intent(out) :: rc
      !
      ! !LOCAL VARIABLES
      character(len=*), parameter :: subname='shr_wtracers_finalize'
      !-----------------------------------------------------------------------

      rc = ESMF_SUCCESS

      if (.not. water_tracers_initialized) then
         call shr_log_error("Attempt to call "//subname//" when water tracers haven't been initialized", rc=rc)
         return
      end if

      num_tracers = 0
      deallocate(tracer_names)
      deallocate(tracer_species_types)
      deallocate(tracer_species_names)
      deallocate(tracer_initial_ratios)
      water_tracers_initialized = .false.
   end subroutine shr_wtracers_finalize

end module shr_wtracers_mod
