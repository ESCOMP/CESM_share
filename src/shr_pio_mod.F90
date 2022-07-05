module shr_pio_mod
  use pio, only : iosystem_desc_t
  use shr_kind_mod, only : shr_kind_cl
  use shr_sys_mod,  only : shr_sys_abort

  implicit none
  private

  public :: shr_pio_getiosys
  public :: shr_pio_getiotype
  public :: shr_pio_getioroot
  public :: shr_pio_getioformat
  public :: shr_pio_getrearranger

  interface shr_pio_getiotype
     module procedure shr_pio_getiotype_fromid, shr_pio_getiotype_fromname
  end interface shr_pio_getiotype
  interface shr_pio_getioformat
     module procedure shr_pio_getioformat_fromid, shr_pio_getioformat_fromname
  end interface shr_pio_getioformat
  interface shr_pio_getiosys
     module procedure shr_pio_getiosys_fromid, shr_pio_getiosys_fromname
  end interface shr_pio_getiosys
  interface shr_pio_getioroot
     module procedure shr_pio_getioroot_fromid, shr_pio_getioroot_fromname
  end interface shr_pio_getioroot
  interface shr_pio_getindex
     module procedure shr_pio_getindex_fromid, shr_pio_getindex_fromname
  end interface shr_pio_getindex
  interface shr_pio_getrearranger
     module procedure shr_pio_getrearranger_fromid, shr_pio_getrearranger_fromname
  end interface shr_pio_getrearranger

  type pio_comp_t
     integer :: compid
     integer :: pio_root
     integer :: pio_stride
     integer :: pio_numiotasks
     integer :: pio_iotype
     integer :: pio_rearranger
     integer :: pio_netcdf_ioformat
  end type pio_comp_t

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! The following variables are public just for the sake of the driver's driver_pio_mod;
  ! these NEED to be set by that module.
  !
  ! No other subroutines should access these directly! Any other access should go
  ! through the above accessor routines.
  character(len=16), allocatable, public :: io_compname(:)
  type(pio_comp_t), allocatable, public :: pio_comp_settings(:)
  type(iosystem_desc_t), allocatable, target, public :: iosystems(:)
  integer, allocatable, public :: io_compid(:)

  ! Similarly, this function is public just for the sake of the driver's driver_pio_mod;
  ! it should not be used elsewhere.
  public :: shr_pio_getindex
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

  function shr_pio_getiotype_fromid(compid) result(io_type)
    integer, intent(in) :: compid
    integer :: io_type

    io_type = pio_comp_settings(shr_pio_getindex(compid))%pio_iotype

  end function shr_pio_getiotype_fromid

  function shr_pio_getiotype_fromname(component) result(io_type)
    ! 'component' must be equal to some element of io_compname(:)
    ! (but it is case-insensitive)
    character(len=*), intent(in) :: component
    integer :: io_type

    io_type = pio_comp_settings(shr_pio_getindex(component))%pio_iotype

  end function shr_pio_getiotype_fromname

  function shr_pio_getrearranger_fromid(compid) result(io_type)
    integer, intent(in) :: compid
    integer :: io_type

    io_type = pio_comp_settings(shr_pio_getindex(compid))%pio_rearranger

  end function shr_pio_getrearranger_fromid

  function shr_pio_getrearranger_fromname(component) result(io_type)
    ! 'component' must be equal to some element of io_compname(:)
    ! (but it is case-insensitive)
    character(len=*), intent(in) :: component
    integer :: io_type

    io_type = pio_comp_settings(shr_pio_getindex(component))%pio_rearranger

  end function shr_pio_getrearranger_fromname

  function shr_pio_getioformat_fromid(compid) result(io_format)
    integer, intent(in) :: compid
    integer :: io_format

    io_format = pio_comp_settings(shr_pio_getindex(compid))%pio_netcdf_ioformat

  end function shr_pio_getioformat_fromid

  function shr_pio_getioformat_fromname(component) result(io_format)
    ! 'component' must be equal to some element of io_compname(:)
    ! (but it is case-insensitive)
    character(len=*), intent(in) :: component
    integer :: io_format

    io_format = pio_comp_settings(shr_pio_getindex(component))%pio_netcdf_ioformat

  end function shr_pio_getioformat_fromname

  function shr_pio_getioroot_fromid(compid) result(io_root)
    ! 'component' must be equal to some element of io_compname(:)
    ! (but it is case-insensitive)
    integer, intent(in) :: compid
    integer :: io_root

    io_root = pio_comp_settings(shr_pio_getindex(compid))%pio_root

  end function shr_pio_getioroot_fromid

  function shr_pio_getioroot_fromname(component) result(io_root)
    ! 'component' must be equal to some element of io_compname(:)
    ! (but it is case-insensitive)
    character(len=*), intent(in) :: component
    integer :: io_root

    io_root = pio_comp_settings(shr_pio_getindex(component))%pio_root

  end function shr_pio_getioroot_fromname


  !! Given a component name, return the index of that component.
  !! This is the index into io_compid, io_compname, comp_pio_iotype, etc.
  !! If the given component is not found, return -1
  integer function shr_pio_getindex_fromid(compid) result(index)
    implicit none
    integer, intent(in) :: compid
    integer :: i
    character(len=shr_kind_cl) :: msg

    index = -1
    do i=1, size(io_compid)
       if(io_compid(i)==compid) then
          index = i
          exit
       end if
    end do

    if(index<0) then
       write(msg, *) 'shr_pio_getindex :: compid=',compid,' out of allowed range: '
       call shr_sys_abort(msg)
    end if
  end function shr_pio_getindex_fromid

  integer function shr_pio_getindex_fromname(component) result(index)
    use shr_string_mod, only : shr_string_toupper

    implicit none

    ! 'component' must be equal to some element of io_compname(:)
    ! (but it is case-insensitive)
    character(len=*), intent(in) :: component

    character(len=len(component)) :: component_ucase
    integer :: i

    ! convert component name to upper case in order to match case in io_compname
    component_ucase = shr_string_toUpper(component)

    index = -1  ! flag for not found
    do i=1,size(io_compname)
       if (trim(component_ucase) == trim(io_compname(i))) then
          index = i
          exit
       end if
    end do
    if(index<0) then
       call shr_sys_abort(' shr_pio_getindex:: compid out of allowed range')
    end if
  end function shr_pio_getindex_fromname

  function shr_pio_getiosys_fromid(compid) result(iosystem)
    ! 'component' must be equal to some element of io_compname(:)
    ! (but it is case-insensitive)
    integer, intent(in) :: compid
    type(iosystem_desc_t), pointer :: iosystem

    iosystem => iosystems(shr_pio_getindex(compid))

  end function shr_pio_getiosys_fromid

  function shr_pio_getiosys_fromname(component) result(iosystem)
    ! 'component' must be equal to some element of io_compname(:)
    ! (but it is case-insensitive)
    character(len=*), intent(in) :: component
    type(iosystem_desc_t), pointer :: iosystem

    iosystem => iosystems(shr_pio_getindex(component))

  end function shr_pio_getiosys_fromname

end module shr_pio_mod
