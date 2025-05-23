MODULE shr_kind_mod

  !----------------------------------------------------------------------------
  ! precision/kind constants add data public
  !----------------------------------------------------------------------------
  
  public
  integer,parameter :: SHR_KIND_R8 = selected_real_kind(12) ! 8 byte real
  integer,parameter :: SHR_KIND_R4 = selected_real_kind( 6) ! 4 byte real
  integer,parameter :: SHR_KIND_RN = kind(1.0)              ! native real
  integer,parameter :: SHR_KIND_I8 = selected_int_kind (13) ! 8 byte integer
  integer,parameter :: SHR_KIND_I4 = selected_int_kind ( 6) ! 4 byte integer
  integer,parameter :: SHR_KIND_I2 = selected_int_kind ( 4) ! 2 byte integer
  integer,parameter :: SHR_KIND_IN = kind(1)                ! native integer
  integer,parameter :: SHR_KIND_CS = 80                     ! short char
  integer,parameter :: SHR_KIND_CM = 160                    ! mid-sized char
  integer,parameter :: SHR_KIND_CL = 256                    ! long char
  integer,parameter :: SHR_KIND_CX = 512                    ! extra-long char
  integer,parameter :: SHR_KIND_CXX= 4096                   ! extra-extra-long char
  real(kind=shr_kind_r8),parameter :: tinyvalue = tiny(1._shr_kind_R8)     ! tiny value
  real(kind=shr_kind_r8),parameter :: hugevalue = huge(1._shr_kind_r8)     ! huge value
END MODULE shr_kind_mod
