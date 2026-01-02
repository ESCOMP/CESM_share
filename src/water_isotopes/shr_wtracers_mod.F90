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
   use shr_kind_mod      , only : CS=>SHR_KIND_CS, CM=>SHR_KIND_CM, CXX=>SHR_KIND_CXX
   use shr_log_mod       , only : shr_log_error
   use shr_log_mod       , only : s_logunit=>shr_log_Unit
   use shr_string_mod    , only : shr_string_listGetAllNames, shr_string_toUpper
   use shr_string_mod    , only : shr_string_withoutSuffix
   use shr_sys_mod       , only : shr_sys_abort
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
   public :: shr_wtracers_finalize          ! finalize water tracer information
   public :: shr_wtracers_is_wtracer_field  ! return true if the given field name is a water tracer field
   public :: shr_wtracers_present           ! return true if there are water tracers in this simulation
   public :: shr_wtracers_get_num_tracers   ! get number of water tracers in this simulation
   public :: shr_wtracers_get_name          ! get the name of a given tracer
   public :: shr_wtracers_get_species_type  ! get the species type value associated with a given tracer
   public :: shr_wtracers_get_species_name  ! get the species name associated with a given tracer
   public :: shr_wtracers_is_isotope        ! return true if a given tracer is an isotope
   public :: shr_wtracers_get_initial_ratio ! get the initial ratio for a given tracer

   !--------------------------------------------------------------------------
   ! Private interfaces
   !--------------------------------------------------------------------------

   private :: shr_wtracers_parse_attributes   ! Parse water tracer NUOPC attributes
   private :: shr_wtracers_set_species_types  ! Set species types from species names
   private :: shr_wtracers_set_initial_ratios ! Set real-valued initial ratios from strings
   private :: shr_wtracers_print              ! Print tracer info to log
   private :: shr_wtracers_check_tracer_num   ! Check a tracer_num argument and abort if invalid

   !--------------------------------------------------------------------------
   ! Public data
   !--------------------------------------------------------------------------

   ! Max length of various strings
   integer, parameter, public :: WTRACER_NAME_MAXLEN = CM
   integer, parameter, public :: WTRACER_SPECIES_NAME_MAXLEN = CM

   ! Species name associated with bulk water tracers
   character(len=*), parameter, public :: WATER_SPECIES_NAME_BULK = "-"

   ! Possible species types
   integer, parameter, public :: WATER_SPECIES_TYPE_UNDEFINED = -1
   integer, parameter, public :: WATER_SPECIES_TYPE_BULK = 0  ! This one is special: total/bulk water rather than a species
   integer, parameter, public :: WATER_SPECIES_TYPE_H218O = 1
   integer, parameter, public :: WATER_SPECIES_TYPE_H217O = 2
   integer, parameter, public :: WATER_SPECIES_TYPE_HDO = 3
   integer, parameter, public :: WATER_SPECIES_TYPE_MAXVAL = 3

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

      call shr_wtracers_print(maintask, rc=rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return

   end subroutine shr_wtracers_init

   !-----------------------------------------------------------------------
   subroutine shr_wtracers_parse_attributes(driver, rc)
      !
      ! !DESCRIPTION:
      ! Parse water tracer NUOPC attributes
      !
      ! This parses three attributes, which are all colon-delimited strings, and which all
      ! must have the same number of elements (this requirement is checked here):
      ! - water_tracer_names (arbitrary user-defined names)
      ! - water_tracer_species (corresponding to predetermined strings like "H218O", or
      !   the string given by WATER_SPECIES_NAME_BULK)
      ! - water_tracer_initial_ratios (strings that are convertable to real numbers)
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
   subroutine shr_wtracers_check_tracer_num(tracer_num, subname)
      !
      ! !DESCRIPTION:
      ! Check a tracer_num argument and abort with a meaningful message if invalid
      !
      ! !ARGUMENTS
      integer, intent(in) :: tracer_num
      character(len=*), intent(in) :: subname  ! name of the caller, for error message
      !-----------------------------------------------------------------------
      if (tracer_num < 1 .or. tracer_num > num_tracers) then
         write(s_logunit, '(A,I0)') subname//" ERROR: tracer_num out of range: ", tracer_num
         if (num_tracers == 0) then
            write(s_logunit, '(A)') "(This simulation has no tracers.)"
         else
            write(s_logunit, '(A,I0,A)') "(Valid range: 1 - ", num_tracers, ".)"
         end if
         call shr_sys_abort(subname//" ERROR: tracer_num out of range")
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

      character(len=*), parameter :: subname='shr_wtracers_is_wtracer_field'
      !-----------------------------------------------------------------------

      call shr_string_withoutSuffix( &
           in_str = fieldname, &
           suffix = WTRACERS_SUFFIX, &
           has_suffix = is_tracer, &
           rc = localrc)
      if (localrc /= 0) then
         call shr_sys_abort(subname//": ERROR in shr_string_withoutSuffix")
      end if
      shr_wtracers_is_wtracer_field = is_tracer
   end function shr_wtracers_is_wtracer_field

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
