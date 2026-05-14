! Stubs of the gptl timing library

! Note that in the real gptl, these are defined in C. Here we define them in Fortran for
! simplicity, but keep them outside of a module for consistency with their usage (via an
! "external" statement rather than a "use" statement).

function GPTLprint_memusage(msg) result(ierr)
   character(len=*), intent(in) :: msg
   integer :: ierr

   ierr = 0
end function GPTLprint_memusage
