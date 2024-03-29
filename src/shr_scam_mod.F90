!===============================================================================
! SVN $Id:
! SVN $URL:
!===============================================================================
!BOP ===========================================================================
!
! !MODULE: shr_scam_mod.F90 --- Module to handle single column mode share routines.
!
! !DESCRIPTION:
!    Routines needed by drv or several component models for running in single column mode
!
! !REVISION HISTORY:
!    2010 Nov 05 - E. Kluzek ---- add PIO and file interfaces for getCloseLatLon
!    2007 Sep 14 - B. Kauffman  - svn checkin
!    2007 Aug 29 - J. Truesdale - first version
!
! !INTERFACE: ------------------------------------------------------------------

module shr_scam_mod

! !USES:

   use shr_abort_mod,  only : abort => shr_abort_abort   ! system calls
   use shr_kind_mod,   only : R8=>SHR_KIND_R8,IN=>SHR_KIND_IN,CL=>SHR_KIND_CL
   use shr_log_mod,    only : s_loglev  => shr_log_Level
   use shr_log_mod,    only : s_logunit => shr_log_Unit

   implicit none

   private           ! By default everything is private to this module

! !PUBLIC TYPES:

   ! no public types

! !PUBLIC MEMBER FUNCTIONS:

   public :: shr_scam_getCloseLatLon ! return lat and lon point/index
!
   interface shr_scam_getCloseLatLon
     module procedure shr_scam_getCloseLatLonNC
     module procedure shr_scam_getCloseLatLonPIO
     module procedure shr_scam_getCloseLatLonFile
   end interface

! !PUBLIC DATA MEMBERS:

   ! no public data members

!EOP

! !PRIVATE  MEMBER FUNCTIONS:

   private :: is_latlon          ! Check if variable name is a latitude or longitude
   private :: get_close          ! Retrieve the closest lat/lon
   private :: get_latlonindices  ! Get the start/count indices to retreive lat or long

! !PRIVATE DATA MEMBERS:

   save

!===============================================================================
CONTAINS
!===============================================================================

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: shr_scam_getCloseLatLonNC
!
! !DESCRIPTION:
!    routine to search in netcdf file and return lat and lon point/index closest to target point
!
! !REVISION HISTORY:
!     2010 Nov 05 - E. Kluzek ---- Use is_latlon/get_close/get_latlonindices routines
!     2007 Aug 29 - J. Truesdale - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine shr_scam_getCloseLatLonNC(ncid, targetLat,  targetLon, closeLat, closeLon, &
                                     closeLatIdx, closeLonIdx, found, rc)
! !USES:
   use netcdf         , only: nf90_max_var_dims, nf90_inquire, nf90_noerr, nf90_inquire_variable
   use netcdf         , only: nf90_inquire_dimension, nf90_get_var
   use shr_ncread_mod, only: shr_ncread_handleErr
   implicit none

! !INPUT/OUTPUT PARAMETERS:

   integer(IN),intent(in)  :: ncid         ! netcdf id
   real   (R8),intent(in)  :: targetLat    ! find closest latitude to this point
   real   (R8),intent(in)  :: targetLon    ! find closest longitude to this point
   real   (R8),intent(out) :: closeLat     ! returned close lat
   real   (R8),intent(out) :: closeLon     ! returned close lon
   integer(IN),intent(out) :: closeLatIdx  ! index of returned lat point
   integer(IN),intent(out) :: closeLonIdx  ! index of returned lon point
   logical, optional, intent(out) :: found ! if found answer (will abort if found NOT sent and
                                           ! it couldn't find the lat/lon dimensions)
   integer, optional, intent(out)   :: rc  ! Return code

!EOP


   !----- local variables -----
   real   (R8),allocatable          :: lats(:),lons(:)
   integer(IN)                      :: rcode   ! netCDF routine return code
   integer(IN)                      ::  len
   integer(IN)                      ::  latlen
   integer(IN)                      ::  lonlen
   integer(IN)                      ::  ndims
   integer(IN)                      ::  nlatdims
   integer(IN)                      ::  nlondims
   integer(IN)                      ::  nvars
   integer(IN)                      ::  nvarid
   integer(IN)                      ::  ndimid
   integer(IN)                      ::  strt(nf90_max_var_dims),cnt(nf90_max_var_dims)
   integer(IN)                      ::  nlon,nlat
   integer(IN), dimension(nf90_max_var_dims) :: dimids
   logical                          ::  lfound        ! local version of found
   character(len=80), allocatable   ::  vars(:)
   character(len=80), allocatable   ::  latdimnames(:)
   character(len=80), allocatable   ::  londimnames(:)
   character(*),parameter :: subname = "(shr_scam_getCloseLatLonNC) "

!-------------------------------------------------------------------------------
! Notes:
!-------------------------------------------------------------------------------

   if ( present(rc) )then
      rc = 0
   end if
   if ( present(found) )then
      lfound = found
   else
      lfound = .false.
   end if
   if ( present(rc) )then
      rc = 0   ! Initialize return code to something
   end if

   !--- Get variable info for search ---

   rcode = nf90_inquire(ncid, nVariables=nvars)
   if (rcode /= nf90_noerr) then
      call shr_ncread_handleErr( rcode, subname//"ERROR from nf90_inquire" )
      if ( present(rc) )then
         rc = rcode
         return
      end if
   endif

   allocate( vars(nvars) )
   do nvarid = 1, nvars
      rcode = nf90_inquire_variable(ncid, nvarid, vars(nvarid), ndims=ndims,dimids = dimids)
      if (rcode /= nf90_noerr) then
         call shr_ncread_handleErr( rcode, subname//"ERROR inquiring about variable "// &
                                    trim(vars(nvarid)) )
         if ( present(rc) )then
            rc = rcode
            return
         end if
      endif
      !-- If latitude variable ---
      if ( is_latlon( vars(nvarid), latitude=.true., varnotdim=.true. ) )then
         nlatdims = ndims
         if (.not. allocated(latdimnames) ) allocate( latdimnames(ndims) )
         do ndimid =  1,ndims
            rcode = nf90_inquire_dimension(ncid, dimids(ndimid), latdimnames(ndimid), len)
            if (rcode /= nf90_noerr) then
               call shr_ncread_handleErr( rcode, subname// &
                   "ERROR: Cant read netcdf latitude variable dimension")
               if ( present(rc) )then
                  rc = rcode
                  return
               end if
            endif
            !--- is this a latitude dimension  ---
            if ( is_latlon( latdimnames(ndimid), latitude=.true., varnotdim=.false. ) )then
               latlen = len
            end if
         end do
      end if
      !-- If longitude variable ---
      if ( is_latlon( vars(nvarid), latitude=.false., varnotdim=.true. ) )then
         nlondims = ndims
         if (.not. allocated(londimnames) ) allocate( londimnames(ndims) )
         do ndimid =  1,ndims
            rcode = nf90_inquire_dimension(ncid, dimids(ndimid), londimnames(ndimid), len)
            call shr_ncread_handleErr( rcode, subname &
                           //"ERROR: Cant read netcdf longitude variable dimension" )
            if ( rcode /= nf90_noerr .and. present(rc) )then
               rc = rcode
               return
            end if
            !--- is this a longitude dimension  ---
            if ( is_latlon( londimnames(ndimid), latitude=.false., varnotdim=.false. ) )then
               lonlen = len
            end if
         end do
      end if
   end do

   !--- Look for/extract lat lon coordinate variables from file ---

   nlat=0
   nlon=0
   nvarid=0

   !--- Loop through all variables until we find lat and lon ---

   do while (nvarid < nvars .and.(nlon.eq.0 .or. nlat.eq.0))
      nvarid=nvarid+1

      !--- Get latitude ---

      if ( is_latlon( vars(nvarid), latitude=.true., varnotdim=.true. ) )then

         call get_latlonindices( latitude=.true., dimnames=latdimnames, ndims=nlatdims, &
                                 nlen=latlen, strt=strt, cnt=cnt )
         nlat = latlen
         if (.not. allocated(lats) ) allocate( lats(nlat) )
         rcode= nf90_get_var(ncid, nvarid ,lats, start = strt, count = cnt)
         call shr_ncread_handleErr( rcode, subname &
                           //"ERROR: Cant read netcdf latitude" )
         if ( rcode /= nf90_noerr .and. present(rc) )then
            rc = rcode
            return
         end if
      end if

      !--- Get longitude ---

      if ( is_latlon( vars(nvarid), latitude=.false., varnotdim=.true. ) )then
         call get_latlonindices( latitude=.false., ndims=nlondims, dimnames=londimnames, &
                                 nlen=lonlen, strt=strt, cnt=cnt )
         nlon = lonlen
         if (.not. allocated(lons) ) allocate( lons(nlon) )
         rcode= nf90_get_var(ncid, nvarid ,lons, start = strt, count = cnt)
         call shr_ncread_handleErr( rcode, subname &
                           //"ERROR: Cant read netcdf longitude" )
         if ( rcode /= nf90_noerr .and. present(rc) )then
            rc = rcode
            return
         end if
      end if
   end do
   if ( present(found) )then
      if ( nlat == 0 .or. nlon == 0 ) then
         write(s_logunit,*) subname//"WARNING: Cant find appropriate latitude or longitude coordinate variables"
         found = .false.
      else
         call get_close( targetLon, targetLat, nlon, lons, nlat, lats, closelonidx, closelatidx, found )
         if ( found )then
            closelon=lons(closelonidx)
            closelat=lats(closelatidx)
         end if
      end if
   else

      call get_close( targetLon, targetLat, nlon, lons, nlat, lats, closelonidx, closelatidx )
      closelon=lons(closelonidx)
      closelat=lats(closelatidx)

   end if

   if (londimnames(1) .eq. 'ncol') then !-- Enforce for SE grids
     closelatidx = 1
   endif

   if (londimnames(1) .eq. 'ncol') then !-- Enforce for SE grids
     closelatidx = 1
   endif

   if ( allocated(lats)        ) deallocate(lats)
   if ( allocated(lons)        ) deallocate(lons)
   if ( allocated(latdimnames) ) deallocate(latdimnames)
   if ( allocated(londimnames) ) deallocate(londimnames)
   if ( allocated(vars)        ) deallocate( vars )

   return

end subroutine shr_scam_getCloseLatLonNC

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: shr_scam_getCloseLatLonPIO
!
! !DESCRIPTION:
!    routine to search in netcdf file and return lat and lon point/index
!    closest to target point using PIO.
!
! !REVISION HISTORY:
!     2010 Nov 01 - E. Kluzek  - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine shr_scam_getCloseLatLonPIO(pioid, targetLat,  targetLon, closeLat, closeLon, &
                                      closeLatIdx, closeLonIdx, found, rc )
   use netcdf,         only: nf90_max_var_dims, nf90_open, nf90_nowrite, nf90_noerr
   use netcdf,         only: nf90_get_var
   use pio,            only: file_desc_t, PIO_BCAST_ERROR, pio_inquire, pio_inquire_variable, PIO_NOERR
   use pio,            only: pio_inquire_dimension, pio_get_var, pio_seterrorhandling, PIO_INTERNAL_ERROR
   use shr_ncread_mod, only: shr_ncread_handleErr
   implicit none

! !INPUT/OUTPUT PARAMETERS:

   type(file_desc_t), intent(inout) :: pioid        ! pio file ID
   real   (R8),       intent(in)    :: targetLat    ! find closest latitude to this point
   real   (R8),       intent(in)    :: targetLon    ! find closest longitude to this point
   real   (R8),       intent(out)   :: closeLat     ! returned close lat
   real   (R8),       intent(out)   :: closeLon     ! returned close lon
   integer(IN),       intent(out)   :: closeLatIdx  ! index of returned lat point
   integer(IN),       intent(out)   :: closeLonIdx  ! index of returned lon point
   logical, optional, intent(out)   :: found        ! if found answer (will abort if found NOT sent and
                                                    ! it couldn't find the lat/lon dimensions)
   integer, optional, intent(out)   :: rc           ! Return code

!EOP


   !----- local variables -----
   real   (R8),allocatable          :: lats(:),lons(:)
   integer(IN)                      :: rcode   ! netCDF routine return code
   integer(IN)                      ::  len      = 0
   integer(IN)                      ::  latlen   = 0
   integer(IN)                      ::  lonlen   = 0
   integer(IN)                      ::  ndims    = 0
   integer(IN)                      ::  nlatdims = 0
   integer(IN)                      ::  nlondims = 0
   integer(IN)                      ::  nvars    = 0
   integer(IN)                      ::  nvarid
   integer(IN)                      ::  ndimid
   integer(IN)                      ::  strt(nf90_max_var_dims),cnt(nf90_max_var_dims)
   integer(IN)                      ::  nlon = 0, nlat = 0
   logical                          ::  lfound
   logical                          ::  is_segrid, islatitude        ! local version of found
   integer(IN), dimension(nf90_max_var_dims) :: dimids
   character(len=80), allocatable   ::  vars(:)
   character(len=80), allocatable   ::  latdimnames(:)
   character(len=80), allocatable   ::  londimnames(:)
   character(*),parameter :: subname = "(shr_scam_getCloseLatLonPIO) "

!-------------------------------------------------------------------------------
! Notes:
!-------------------------------------------------------------------------------

   if ( present(found) )then
      lfound = found
   else
      lfound = .false.
   end if
   if ( present(rc) )then
      rc = 0   ! Initialize return code to something
   end if
   !--- Get variable info for search ---

   call pio_seterrorhandling(pioid,PIO_BCAST_ERROR)
   rcode = pio_inquire(pioid, nVariables=nvars)
   if (rcode /= PIO_noerr) then
      call shr_ncread_handleErr( rcode, subname//"ERROR: from PIO_inquire ")
      if ( present(rc) )then
         rc = rcode
         return
      end if
   endif

   allocate( vars(nvars) )
   do nvarid = 1, nvars
      rcode = pio_inquire_variable(pioid, nvarid, vars(nvarid), ndims=ndims,dimids = dimids)
      if (rcode /= PIO_noerr) then
         write(s_logunit,*) subname//"ERROR inquiring about variable id #", nvarid
         call shr_ncread_handleErr( rcode, subname//"ERROR: inquiring about variable" )
         if ( present(rc) )then
            rc = rcode
            return
         end if
      endif
      !-- If latitude variable ---
      if ( is_latlon( vars(nvarid), latitude=.true., varnotdim=.true. ) )then
         nlatdims = ndims
         allocate( latdimnames(ndims) )
         do ndimid =  1,ndims
            rcode = pio_inquire_dimension(pioid, dimids(ndimid), latdimnames(ndimid), len)
            if (rcode /= pio_noerr) then
               call shr_ncread_handleErr( rcode, subname// &
                                   "ERROR: Cant read netcdf latitude variable dimension")
               if ( present(rc) )then
                  rc = rcode
                  return
               end if
            endif
            !--- is this a latitude dimension  ---
            if ( is_latlon( latdimnames(ndimid), latitude=.true., varnotdim=.false. ) )then
               latlen = len
            end if
         end do
      end if
      !-- If longitude variable ---
      if ( is_latlon( vars(nvarid), latitude=.false., varnotdim=.true. ) )then
         nlondims = ndims
         allocate( londimnames(ndims) )
         do ndimid =  1,ndims
            rcode = pio_inquire_dimension(pioid, dimids(ndimid), londimnames(ndimid), len)
            if (rcode /= PIO_noerr) then
               call shr_ncread_handleErr( rcode, subname// &
                                   "ERROR: Cant read netcdf longitude variable dimension")
               if ( present(rc) )then
                  rc = rcode
                  return
               end if
            endif
            !--- is this a longitude dimension  ---
            if ( is_latlon( londimnames(ndimid), latitude=.false., varnotdim=.false. ) )then
               lonlen = len
            end if
         end do
      end if
   end do

   !--- Look for/extract lat lon coordinate variables from file ---

   nlat=0
   nlon=0
   nvarid=0

   if (latlen .eq. 1 .and. lonlen .gt. 1) then
     latlen=lonlen
     islatitude=.false. ! if spectral element lat and lon
                        !   are on same array structure
     is_segrid=.true.
   else if ( latlen==lonlen ) then
     islatitude=.false. ! if spectral element lat and lon
                        !   are on same array structure
     is_segrid=.true.
  else
     islatitude=.true.
     is_segrid=.false.
   endif

   !--- Loop through all variables until we find lat and lon ---

   do while (nvarid < nvars .and.(nlon.eq.0 .or. nlat.eq.0))
      nvarid=nvarid+1

      !--- Get latitude ---

      if ( is_latlon( vars(nvarid), latitude=.true., varnotdim=.true. ) )then

         call get_latlonindices( latitude=islatitude, ndims=nlatdims, dimnames=latdimnames, &
                                 nlen=latlen, strt=strt, cnt=cnt )

         nlat = latlen
         allocate(lats(nlat))
         rcode= pio_get_var(pioid, nvarid ,strt(:nlatdims), cnt(:nlatdims), lats)
         if (rcode /= PIO_noerr) then
            call shr_ncread_handleErr( rcode, subname// &
                               "ERROR: Cant read netcdf latitude")
            if ( present(rc) )then
               rc = rcode
               return
            end if
         endif
      end if

      !--- Get longitude ---

      if ( is_latlon( vars(nvarid), latitude=.false., varnotdim=.true. ) )then
         call get_latlonindices( latitude=.false., ndims=nlondims, dimnames=londimnames, &
                                 nlen=lonlen, strt=strt, cnt=cnt )
         nlon = lonlen
         allocate(lons(nlon))
         rcode= pio_get_var(pioid, nvarid ,strt(:nlondims), cnt(:nlondims), lons)
         if (rcode /= PIO_noerr) then
            call shr_ncread_handleErr( rcode, subname// &
                               "ERROR: Cant read netcdf longitude")
            if ( present(rc) )then
               rc = rcode
               return
            end if
         endif
      end if
   end do
   call pio_seterrorhandling(pioid,PIO_INTERNAL_ERROR)

   if ( present(found) )then
      if ( nlat == 0 .or. nlon == 0 ) then
         write(s_logunit,*) subname//"WARNING: Cant find appropriate latitude or longitude coordinate variables"
         found = .false.
      else
         call get_close( targetLon, targetLat, nlon, lons, nlat, lats, closelonidx, closelatidx, found )
         if ( found )then
            closelon=lons(closelonidx)
            closelat=lats(closelatidx)
         end if
      end if
   else
      call get_close( targetLon, targetLat, nlon, lons, nlat, lats, closelonidx, closelatidx )
      closelon=lons(closelonidx)
      closelat=lats(closelatidx)
   end if
   if ( allocated(lats) ) deallocate(lats)
   if ( allocated(lons) ) deallocate(lons)
   deallocate( vars )

   !--- If dealing with SE grids, set this to 1
   if (is_segrid) then
     closelatidx = 1
   endif

   return
end subroutine shr_scam_getCloseLatLonPIO

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: shr_scam_getCloseLatLonFile
!
! !DESCRIPTION:
!    routine to search in netcdf file and return lat and lon point/index closest to target point
!
! !REVISION HISTORY:
!     2010 Oct 27 - E. Kluzek - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine shr_scam_getCloseLatLonFile(filename, targetLat,  targetLon, closeLat, closeLon, &
                                       closeLatIdx, closeLonIdx, found, rc)
! !USES:
   use shr_ncread_mod, only: shr_ncread_open, shr_ncread_close
   use netcdf, only : nf90_noerr
   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(len=*),intent(in)    :: filename     ! Input NetCDF filename
   real   (R8),intent(in)         :: targetLat    ! find closest latitude to this point
   real   (R8),intent(in)         :: targetLon    ! find closest longitude to this point
   real   (R8),intent(out)        :: closeLat     ! returned close lat
   real   (R8),intent(out)        :: closeLon     ! returned close lon
   integer(IN),intent(out)        :: closeLatIdx  ! index of returned lat point
   integer(IN),intent(out)        :: closeLonIdx  ! index of returned lon point
   logical, optional, intent(out) :: found        ! if found answer (will abort if found NOT sent and
                                                  ! it couldn't find the lat/lon dimensions)
   integer, optional, intent(out) :: rc           ! Return code

!EOP


   !----- local variables -----
   integer :: ncid    ! NetCDF file ID
   integer :: rCode   ! return code
   character(*),parameter :: subname = "(shr_scam_getCloseLatLonFile) "

!-------------------------------------------------------------------------------
! Notes:
!-------------------------------------------------------------------------------
   call shr_ncread_open(fileName,ncid,rCode)
   if ( rCode /= NF90_NOERR )then
      found = .false.
      if ( present(rc) ) rc = rCode
      return
   end if
   call shr_scam_getCloseLatLonNC(ncid, targetLat,  targetLon, closeLat, closeLon, &
                                  closeLatIdx, closeLonIdx, found=found, rc=rCode)
   if ( rCode /= 0 )then
      if ( present(rc) ) rc = rCode
      return
   end if
   call shr_ncread_close(ncid,rCode)
   if ( rCode /= NF90_NOERR )then
      if ( present(rc) ) rc = rCode
   end if

end subroutine shr_scam_getCloseLatLonFile

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: is_latlon
!
! !DESCRIPTION:
!
!     Returns true if the given variable name is a valid latitude or longitude
!     name. The logical input variable latitude is a flag to indicate if you are
!     checking for latitude or longitude variable names.
!
! !REVISION HISTORY:
!     2010 Oct 27 - E. Kluzek  - first version
!
! !INTERFACE: ------------------------------------------------------------------
logical function is_latlon( var_name, latitude, varnotdim )
! !USES:
  use shr_string_mod, only: shr_string_toLower

! !INPUT/OUTPUT PARAMETERS:
    implicit none
    character(len=*), intent(in) :: var_name  ! Input variable name
    logical,          intent(in) :: latitude  ! Flag, true if you want a latitude variable
                                              ! if false check for longitude
    logical,          intent(in) :: varnotdim ! Flag, true if this is a variable
                                              ! and NOT a dimension
!EOP

   !----- local variables -----
   character(len=3)  :: xyvar    ! Variable name for 2D x-y coordinate variables
   character(len=11) :: gcvar    ! Variable name for gridcell coordinate variables
   character(len=CL) :: lowervar ! Lower case variable name
!-------------------------------------------------------------------------------
! Notes:
!-------------------------------------------------------------------------------
    lowervar=shr_string_toLower(trim(var_name))
    is_latlon = .false.
    if ( latitude )then
      if ( varnotdim )then
         xyvar    = "yc"
         gcvar    = "grid1d_lat"
      else
         xyvar    = "nj"
         gcvar    = "gridcell"
      end if
      if ( trim(lowervar) == 'lat'          .or. trim(lowervar) == 'latixy'    .or. &
           trim(lowervar) == trim(xyvar)    .or. trim(lowervar) == 'lsmlat'    .or. &
           trim(lowervar) == trim(gcvar)    .or. trim(lowervar) == 'lat_d'     .or. &
           trim(lowervar) == 'ncol'         .or. trim(lowervar) == 'ncol_d'  &
           )  then
           is_latlon = .true.
      else
           is_latlon = .false.
      end if
    else
      if ( varnotdim )then
         xyvar    = "xc"
         gcvar    = "grid1d_lon"
      else
         xyvar    = "ni"
         gcvar    = "gridcell"
      end if
      if ( trim(lowervar) == 'lon'          .or. trim(lowervar) == 'longxy'    .or. &
           trim(lowervar) == trim(xyvar)    .or. trim(lowervar) == 'lsmlon'    .or. &
           trim(lowervar) == trim(gcvar)    .or. trim(lowervar) == 'lon_d'     .or. &
           trim(lowervar) == 'ncol'         .or. trim(lowervar) == 'ncol_d' )  then
           is_latlon = .true.
      else
           is_latlon = .false.
      end if
    end if
    return
end function is_latlon

!===============================================================================

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: get_latlonindices
!
! !DESCRIPTION:
!        Get the start and count indices to retreive latitude or longitude
!
! !REVISION HISTORY:
!     2010 Nov 03 - E. Kluzek  - first version
!
! !INTERFACE: ------------------------------------------------------------------
subroutine get_latlonindices( latitude, ndims, dimnames, nlen, strt, cnt )
! !USES:
! !INPUT/OUTPUT PARAMETERS:
   implicit none
   logical,     intent(IN)  :: latitude            ! If this is latitude or not (long)
   integer(IN), intent(IN)  :: ndims               ! Number of dimensions
   character(len=*), intent(IN) :: dimnames(ndims) ! Dimension names
   integer(IN), intent(IN)  :: nlen                ! Dimension length
   integer(IN), intent(OUT) :: strt(ndims)         ! Start along dimension
   integer(IN), intent(OUT) :: cnt(ndims)          ! Count along dimension
!EOP

   !----- local variables -----
   integer(IN)   ::  ndimid
   logical       :: found = .false.
   character(*),parameter :: subname = "(shr_scam_getlatlonindices) "
!-------------------------------------------------------------------------------
! Notes:
!-------------------------------------------------------------------------------

   if ( ndims == 0 )then
      call abort( subname//"ERROR: Could NOT find dimension")
   end if
   if ( nlen  == 0 )then
      call abort( subname//"ERROR: Could NOT find dimension length")
   end if
   do ndimid =  1, ndims
      !--- is this a lat/longitude dimension  ---
      if ( is_latlon( dimnames(ndimid), latitude=latitude, varnotdim=.false. ) )then
         strt(ndimid)  = 1
         cnt(ndimid) = nlen
         found         = .true.
      else
         strt(ndimid)  = 1
         cnt(ndimid) = 1
      endif
   end do
   if (.not. found ) then
      if ( latitude )then
         call abort( subname//"ERROR: Cant find a useable latitude dimension" )
      else
         call abort( subname//"ERROR: Cant find a useable longitude dimension")
      end if
   end if
end subroutine get_latlonindices

!===============================================================================

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: get_close
!
! !DESCRIPTION:
!           Get the close latitude and longitude indices for latitude/longitude.
!
! !REVISION HISTORY:
!     2010 Nov 03 - E. Kluzek  - first version
!
! !INTERFACE: ------------------------------------------------------------------
subroutine get_close( targetlon, targetlat, nlon, lons, nlat, lats, closelonidx, &
                      closelatidx, found )

! !USES:
! !INPUT/OUTPUT PARAMETERS:
   implicit none
   real   (R8),intent(in)   :: targetLon    ! find closest longitude to this point
   real   (R8),intent(in)   :: targetLat    ! find closest latitude to this point
   integer(IN),intent(in)   :: nlon         ! Number of longitudes
   real   (R8),intent(in)   :: lons(nlon)   ! Longitude array
   integer(IN),intent(in)   :: nlat         ! Number of latitudes
   real   (R8),intent(in)   :: lats(nlat)   ! Latitude array
   integer(IN),intent(out)  :: closeLatIdx  ! index of returned lat point
   integer(IN),intent(out)  :: closeLonIdx  ! index of returned lon point
   logical, optional, intent(out):: found   ! if found answer (will abort if found NOT sent and
                                            ! it couldn't find the lat/lon dimensions)
!EOP

   !----- local variables -----
   real   (R8),allocatable  :: poslons(:)
   real   (R8)              :: postargetlon
   character(*),parameter :: subname = "(shr_scam_getclose) "
   real   (R8) :: minpoint, testpoint
   integer :: n
!-------------------------------------------------------------------------------
! Notes:
!-------------------------------------------------------------------------------
   if ( present(found) )then
      found = .true.
   end if

   !--- Did we get find valid lat and lon coordinate variables ---

   if (nlon == 0) then
      write(s_logunit,*) subname//"ERROR: Coudnt find longitude coordinate"
      if ( present(found) )then
         found = .false.
         return
      else
         call abort( subname//"ERROR: Couldnt find a longitude coordinate variable")
      end if
   end if
   if (nlat == 0) then
      write(s_logunit,*) subname//"ERROR: Coudnt find latitude coordinate"
      if ( present(found) )then
         found = .false.
         return
      else
         call abort( subname//"ERROR: Couldnt find a latitude coordinate variable")
      end if
   end if
   !--- Convert target latitude to within 0-360 ---
   postargetlon=mod(targetlon+360._r8,360._r8)

   !--- Make sure target latitude within globe ---
   if ( targetlat < -90.0_r8 .or. targetlat > 90.0_r8 )then
      write(s_logunit,*) subname//"ERROR: target latitude out of range = ", targetlat
      if ( present(found) )then
         found = .false.
         return
      else
         call abort( subname//"ERROR: target latitude out of reasonable range")
      end if
   end if

   !--- convert lons array and targetlon to 0,360 ---

   allocate(poslons(nlon))
   poslons=mod(lons+360._r8,360._r8)

   !--- find index of value closest to 0 and set returned values ---

   !--- if SCM uses Eulerian grids
   if (nlat .ne. nlon) then

     closelonidx=(MINLOC(abs(poslons-postargetlon),dim=1))
     closelatidx=(MINLOC(abs(lats-targetlat),dim=1))

   else !--- if SCM uses SE grids

     minpoint=1000.0
     do n = 1, nlon
       testpoint=abs(poslons(n)-postargetlon)+abs(lats(n)-targetlat)
       if (testpoint .lt. minpoint) then
         minpoint=testpoint
         closelonidx=n
       endif
     enddo
     closelatidx=closelonidx ! yes these are set equal since in SE files
                             !  lat/lon are on same array

   endif

   !--- if it gets here we need to clean up after ourselves ---

   deallocate(poslons)
   return
end subroutine get_close

!===============================================================================

end module shr_scam_mod
