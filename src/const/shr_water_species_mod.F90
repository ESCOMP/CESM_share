module shr_water_species_mod

   !---------------------------------------------------------------------
   !
   ! Purpose:
   !
   ! This module defines integer identifiers for the possible water species types: bulk
   ! water and the various water isotopes. These identifiers are shared between the water
   ! tracer configuration code (shr_wtracers_mod) and the water isotope physics code
   ! (shr_wiso_mod).
   !---------------------------------------------------------------------

   implicit none
   private

   ! Possible species types
   integer, parameter, public :: WATER_SPECIES_TYPE_UNDEFINED = -1
   integer, parameter, public :: WATER_SPECIES_TYPE_BULK = 0  ! This one is special: total/bulk water rather than a species
   integer, parameter, public :: WATER_SPECIES_TYPE_H218O = 1
   integer, parameter, public :: WATER_SPECIES_TYPE_H217O = 2
   integer, parameter, public :: WATER_SPECIES_TYPE_HDO = 3
   integer, parameter, public :: WATER_SPECIES_TYPE_MAXVAL = 3

end module shr_water_species_mod
