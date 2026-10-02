module shr_wiso_mod
!-----------------------------------------------------------------------
!
! This module provides the functions and constants needed to calculate
! the isotopic fractionation of water during phase changes across all
! modeled components of the Earth system.
!
! This module also provides a specialized routine for calculating the isotopic
! flux during ocean/atmosphere exchanges, which is not currently owned by
! a specific component model (at least in CESM).
!
!-----------------------------------------------------------------------

  use shr_kind_mod,     only: r8 => shr_kind_r8

  implicit none
  private

!++++++++++++++++++
! Public interfaces
!++++++++++++++++++

  ! Generic equilibrium fractionation routines (used by most/all component models):

  public :: wiso_liq_vap_equil_frac_factor ! Function for calculating liquid/vapor equilibrium fractionation factor
  public :: wiso_ice_vap_equil_frac_factor ! Function for calculating ice/vapor equilibrium fractionation factor

  ! Generic isotope property query routines (used by most/all component models):

  public :: wiso_get_diffusivity_ratio     ! Function for querying the water isotope molecular diffusivity ratio

  ! Kinetic fractionation routines (currently used only by the atm/ocn flux modules):

  public :: icam_atm_ocn_kinetic_frac_factor ! Function for calculating kinetic fractionation factor for isotopic atm/ocn fluxes.

!=======================================================================

contains

!----------------------
!Fractionation routines:
!----------------------

!=======================================================================
! Liquid/Vapor equilibrium fractionation functions
!=======================================================================

  function wiso_liq_vap_equil_frac_factor(isp,tk) result(equil_frac)

    !-----------------------------------------------------------------------
    ! Public function that returns liquid/vapor equilibrium
    ! fractionation factor given the temperature and a
    ! water isotope (isotopologue) species index.
    !-----------------------------------------------------------------------

    use shr_kind_mod, only: cl=>shr_kind_cl
    use shr_sys_mod,  only: shr_sys_abort

    use shr_wtracers_mod, only: WATER_SPECIES_TYPE_BULK
    use shr_wtracers_mod, only: WATER_SPECIES_TYPE_H218O
    use shr_wtracers_mod, only: WATER_SPECIES_TYPE_H217O
    use shr_wtracers_mod, only: WATER_SPECIES_TYPE_HDO

    ! Function input arguements
    integer , intent(in)        :: isp  ! water species type index (e.g., H2O, HDO, H218O)
    real(r8), intent(in)        :: tk   ! Temperature [K]

    ! Return value (equilibrium fractionation factor)
    real(r8) :: equil_frac

    ! Local variable to save 18O fractionation factor for 17O calculation
    real(r8) :: alpha_18O

    ! Character array to store abort error message
    character(len=cl) :: abort_msg

    !-----------------------------------------------------------------------

    ! Initialize the fractionation factor to a huge negative (unphysical) value:
    equil_frac = huge(-1._r8)

    select case  (isp)
      case(WATER_SPECIES_TYPE_BULK)
        ! No fractionation for H2O
        equil_frac = 1._r8
      case (WATER_SPECIES_TYPE_H218O)
        ! Equation 6 in Horita and Wesolowski, 1994
        equil_frac = horita_wesolowski_frac_factor_18O(tk)
      case (WATER_SPECIES_TYPE_H217O)
        ! Equation 6 in Horita and Wesolowski, 1994
        alpha_18O = horita_wesolowski_frac_factor_18O(tk)
        ! Equation 3 from Barkan and Luz, 2005
        equil_frac = barkan_luz_frac_factor_17O(alpha_18O)
      case (WATER_SPECIES_TYPE_HDO)
        ! Equation 5 in Horita and Wesolowski, 1994
        equil_frac = horita_wesolowski_frac_factor_HDO(tk)
      case default
        ! This situation shouldn't happen, so abort the run
        write(abort_msg,'(a,i0)') 'wiso_liq_vap_equil_frac_factor: ERROR: bad isotope species index; bad index = ', isp
        call shr_sys_abort(abort_msg)
    end select

  end function wiso_liq_vap_equil_frac_factor

!++++++++

  pure function horita_wesolowski_frac_factor_HDO(tk) result(equil_frac_HDO)

    !-----------------------------------------------------------------------
    ! Calculate liquid/vapor equilibrium fractionation for HDO
    !
    ! Citation:
    !
    ! Equation 5 in:
    !
    ! Horita, J. and D. J. Wesolowski,
    ! Liquid-vapor fractionation of oxygen and hydrogen isotopes of water from the freezing to the critical temperature
    ! Geochimica et Cosmochimica Acta, 58, 16, August 1994
    ! DOI: 10.1016/0016-7037(94)90096-5
    !
    !-----------------------------------------------------------------------

    ! Function input arguements
    real(r8), intent(in)  :: tk   ! Temperature [K]

    ! Return value (HDO equilibrium fractionation factor)
    real(r8) :: equil_frac_HDO

    ! Equation 5 (HDO) parameters:
    real(r8), parameter :: &
      alpal_HDO = 1158.8e-12_r8, &
      alpbl_HDO = -1620.1e-9_r8, &
      alpcl_HDO = 794.84e-6_r8, &
      alpdl_HDO = -161.04e-3_r8, &
      alpel_HDO = 2.9992e+6_r8

    !-----------------------------------------------------------------------

    ! Equation 5 in Horita and Wesolowski, 1994
    equil_frac_HDO = exp(alpal_HDO*tk**3 + alpbl_HDO*tk**2 + alpcl_HDO*tk + alpdl_HDO + alpel_HDO/tk**3)

  end function horita_wesolowski_frac_factor_HDO

!++++++++

  pure function horita_wesolowski_frac_factor_18O(tk) result(equil_frac_18O)

    !-----------------------------------------------------------------------
    ! Calculate liquid/vapor equilibrium fractionation for H218O
    !
    ! Citation:
    !
    ! Equation 6 in:
    !
    ! Horita, J. and D. J. Wesolowski,
    ! Liquid-vapor fractionation of oxygen and hydrogen isotopes of water from the freezing to the critical temperature
    ! Geochimica et Cosmochimica Acta, 58, 16, August 1994
    ! DOI: 10.1016/0016-7037(94)90096-5
    !
    !-----------------------------------------------------------------------

    ! Function input arguements
    real(r8), intent(in)  :: tk   ! Temperature [K]

    ! Return value (H218O equilibrium fractionation factor)
    real(r8) :: equil_frac_18O

    ! Equation 6 (18O) parameters:
    real(r8), parameter ::       &
      alpal_18O = 0.35041e+6_r8, &
      alpbl_18O = -1.6664e+3_r8, &
      alpcl_18O = 6.7123_r8,     &
      alpdl_18O = -7.685e-3_r8
    !-----------------------------------------------------------------------

    ! Equation 6 in Horita and Wesolowski, 1994
    equil_frac_18O = exp(alpal_18O/tk**3 + alpbl_18O/tk**2 + alpcl_18O/tk + alpdl_18O)

  end function horita_wesolowski_frac_factor_18O

!=======================================================================
! Ice/Vapor equilibrium fractionation functions
!=======================================================================

  function wiso_ice_vap_equil_frac_factor(isp,tk) result(equil_frac)

    !-----------------------------------------------------------------------
    ! Public function that returns ice/vapor equilibrium
    ! fractionation factor given the temperature and a
    ! water isotope (isotopologue) species index.
    !-----------------------------------------------------------------------

    use shr_kind_mod, only: cl=>shr_kind_cl
    use shr_sys_mod,  only: shr_sys_abort

    use shr_wtracers_mod, only: WATER_SPECIES_TYPE_BULK
    use shr_wtracers_mod, only: WATER_SPECIES_TYPE_H218O
    use shr_wtracers_mod, only: WATER_SPECIES_TYPE_H217O
    use shr_wtracers_mod, only: WATER_SPECIES_TYPE_HDO

    ! Function input arguements
    integer , intent(in)        :: isp  ! water species type index (e.g., H2O, HDO, H218O)
    real(r8), intent(in)        :: tk   ! Temperature [K]

    ! Return value (equilibrium fractionation factor)
    real(r8) :: equil_frac

    ! Local variable to save 18O fractionation factor for 17O calculation
    real(r8) :: alpha_18O

    ! Character array to store abort error message
    character(len=cl) :: abort_msg

    !-----------------------------------------------------------------------

    ! Initialize the fractionation factor to a huge negative (unphysical) value:
    equil_frac = huge(-1._r8)

    select case  (isp)
      case(WATER_SPECIES_TYPE_BULK)
        ! No fractionation for H2O
        equil_frac = 1._r8
      case (WATER_SPECIES_TYPE_H218O)
        ! Equation from Majoube, 1971
        equil_frac = majoube_frac_factor_18O(tk)
      case (WATER_SPECIES_TYPE_H217O)
        ! 18O fractionation factor from Majoube, 1971
        alpha_18O = majoube_frac_factor_18O(tk)
        ! Equation 3 from Barkan and Luz, 2005
        equil_frac = barkan_luz_frac_factor_17O(alpha_18O)
      case (WATER_SPECIES_TYPE_HDO)
        ! Equation 5 in Merlivat and Nief, 1967
        equil_frac = merlivat_nief_frac_factor_HDO(tk)
      case default
        ! This situation shouldn't happen, so abort the run
        write(abort_msg,'(a,i0)') 'wiso_ice_vap_equil_frac_factor: ERROR: bad isotope species index; bad index = ', isp
        call shr_sys_abort(abort_msg)
    end select

  end function wiso_ice_vap_equil_frac_factor

!++++++++

  pure function merlivat_nief_frac_factor_HDO(tk) result(equil_frac_HDO)

    !-----------------------------------------------------------------------
    ! Calculate ice/vapor equilibrium fractionation for HDO
    !
    ! Citation:
    !
    ! Equation 5 in:
    !
    ! Merlivat, L. and G. Nief,
    ! Isotopic fractionation during change of state solid-vapour and liquid-vapour of water at temperatures below 0 degree C
    ! Tellus, 19, 122-126, February 1967
    ! DOI: 10.3402/tellusa.v19i1.9756
    !
    !-----------------------------------------------------------------------

    ! Function input arguements
    real(r8), intent(in)  :: tk   ! Temperature [K]

    ! Return value (HDO equilibrium fractionation factor)
    real(r8) :: equil_frac_HDO

    ! Equation 5 (HDO) parameters:
    real(r8), parameter ::   &
      alpal_HDO = -9.45e-2_r8, &
      alpbl_HDO = 16289._r8

    !-----------------------------------------------------------------------

    ! Equation 5 in Merlivat and Nief, 1967
    equil_frac_HDO = exp(alpal_HDO + alpbl_HDO/tk**2)

  end function merlivat_nief_frac_factor_HDO

!++++++++

  pure function majoube_frac_factor_18O(tk) result(equil_frac_18O)

    !-----------------------------------------------------------------------
    ! Calculate ice/vapor equilibrium fractionation for H218O
    !
    ! Citation:
    !
    ! Majoube, M.
    ! Fractionnement en oxygene 18 et en deutérium entre l'eau et sa vapeur
    ! Journal de Chimie Physique, 68, 1423–1436, 1971
    ! DOI: 10.1051/jcp/1971681423
    !
    !-----------------------------------------------------------------------

    ! Function input arguements
    real(r8), intent(in)  :: tk   ! Temperature [K]

    ! Return value (H218O equilibrium fractionation factor)
    real(r8) :: equil_frac_18O

    ! H218O equation parameters:
    real(r8), parameter ::       &
      alpal_18O = -28.224e-3_r8, &
      alpbl_18O = 11.839_r8

    !-----------------------------------------------------------------------

    ! Equation from Majoube, 1971
    equil_frac_18O = exp(alpal_18O + alpbl_18O/tk)

  end function majoube_frac_factor_18O

!=======================================================================
! H217O equilibrium fractionation function
!=======================================================================

  pure function barkan_luz_frac_factor_17O(equil_factor_18O) result(equil_frac_17O)

    !-----------------------------------------------------------------------
    ! Calculate equilibrium fractionation factor for H217O (all phase changes),
    ! given the H218O equilibrium fraction factor.
    !
    ! Citation:
    !
    ! Equation 3 in:
    !
    ! Barkan, E. and Luz, B.,
    ! High precision measurements of 17O/16O and 18O/16O ratios in H2O
    ! Rapid Communications in Mass Spectrometry, 19, 3737-3742, November 2005
    ! DOI: 10.1002/rcm.2250
    !
    !-----------------------------------------------------------------------

    ! Function input arguements
    real(r8), intent(in) :: equil_factor_18O      ! Equilibrium fractionation factor for H218O.

    ! Return value (H217O equilibrium fractionation factor)
    real(r8) :: equil_frac_17O

    ! Equation 3 parameters:
    real(r8), parameter :: theta = 0.529_r8

    !-----------------------------------------------------------------------

    equil_frac_17O = equil_factor_18O**theta

  end function barkan_luz_frac_factor_17O

!=======================================================================

!-------------------------------------
!Isotope property query routines:
!-------------------------------------

!=======================================================================

  function wiso_get_diffusivity_ratio(isp) result(diff_ratio)

    !-----------------------------------------------------------------------
    ! Public function that returns the molecular diffusivity ratio of a
    ! water isotopologue relative to H216O, given a water isotope species index.
    !-----------------------------------------------------------------------

    use shr_kind_mod, only: cl=>shr_kind_cl
    use shr_sys_mod,  only: shr_sys_abort

    use shr_wtracers_mod, only: WATER_SPECIES_TYPE_BULK
    use shr_wtracers_mod, only: WATER_SPECIES_TYPE_H218O
    use shr_wtracers_mod, only: WATER_SPECIES_TYPE_H217O
    use shr_wtracers_mod, only: WATER_SPECIES_TYPE_HDO

    ! Function input arguments
    integer, intent(in) :: isp  ! water species type index (e.g., H2O, HDO, H218O)

    ! Return value (molecular diffusivity ratio relative to H216O)
    real(r8) :: diff_ratio

    ! Character array to store abort error message
    character(len=cl) :: abort_msg

    ! Diffusivity ratios for HDO and H218O relative to H216O, values from:

    ! Merlivat, L.,
    ! Molecular diffusivities of H216O, HD16O, and H218O in gases
    ! Journal of Chemical Physics, 69, 2864-2871, September 1978
    ! DOI: 10.1063/1.436884

    real(r8), parameter :: DIFF_RATIO_HDO   = 0.9757_r8
    real(r8), parameter :: DIFF_RATIO_H218O = 0.9727_r8

    ! Diffusivity ratio for H217O relative to H216O, exponent value from:

    ! Barkan, E. and B. Luz,
    ! Diffusivity fractionations of H216O/H217O and H216O/H218O in air and their implications for isotope hydrology
    ! Rapid Communications in Mass Spectrometry, 21, 2999-3005, August 2007
    ! DOI: 10.1002/rcm.3180

    real(r8), parameter :: DIFF_RATIO_H217O = DIFF_RATIO_H218O**0.5185_r8

    !-----------------------------------------------------------------------

    ! Initialize the diffusion ratio to a huge negative (unphysical) value:
    diff_ratio = huge(-1._r8)

    select case (isp)
      case(WATER_SPECIES_TYPE_BULK)
        ! No kinetic fractionation for H2O
        diff_ratio = 1._r8
      case (WATER_SPECIES_TYPE_H218O)
        diff_ratio = DIFF_RATIO_H218O
      case (WATER_SPECIES_TYPE_H217O)
        diff_ratio = DIFF_RATIO_H217O
      case (WATER_SPECIES_TYPE_HDO)
        diff_ratio = DIFF_RATIO_HDO
      case default
        ! This situation shouldn't happen, so abort the run
        write(abort_msg,'(a,i0)') 'wiso_get_diffusivity_ratio: ERROR: bad isotope species index; bad index = ', isp
        call shr_sys_abort(abort_msg)
    end select

  end function wiso_get_diffusivity_ratio

!=======================================================================

!-------------------------------------
!Kinetic fractionation functions:
!-------------------------------------

!=======================================================================

  pure function icam_atm_ocn_kinetic_frac_factor(rbot, zbot, zoq, ustar, diff_ratio) result(alpkn)

  !-----------------------------------------------------------------------
  !
  ! Function to calculate the kinetic fractionation factor for
  ! ocean evaporation/condensation, as used in the original iCAM5 implementation.
  !
  ! Citation:
  !
  ! Equations 4 to 10 in:
  !
  ! Nusbaumer, J., Wong, T. E., Bardeen, C., and D. Noone.
  ! Evaluating hydrological processes in the Community Atmosphere Model Version 5 (CAM5)
  ! using stable isotope ratios of water
  ! Journal of Advances in Modeling Earth Systems, 9, 2, 949-977, June 2017
  ! DOI: 10.1002/2016MS000839
  !
  ! Note:  The roughness length now comes from the atm/ocn flux scheme, and
  !        is no longer calculated using equation 7 from Nusbaumer et al., 2017
  !
  !-----------------------------------------------------------------------
    use shr_const_mod, only: gravit=>shr_const_g
    use shr_const_mod, only: karman=>shr_const_karman

    ! Note:  Ideally these quantities would be calculate from
    !        state variables or acquired from the atm/ocn scheme itself,
    !        but for now just hard code it.
    real(r8), parameter :: difair = 2.36e-5_r8          ! molecular diffusivity of air
    real(r8), parameter :: muair  = 1.7e-5_r8           ! dynamic viscosity of air
                                                        ! about 17 degC, 1.73 at STP (Salby)

    !---------------------------- Arguments --------------------------------
    real(r8), intent(in)  :: rbot       ! density of lowest layer [kg m-3]
    real(r8), intent(in)  :: zbot       ! height of lowest atmospheric layer [m]
    real(r8), intent(in)  :: zoq        ! surface roughness length for water [m]
    real(r8), intent(in)  :: ustar      ! friction velocity [m s-1]
    real(r8), intent(in)  :: diff_ratio ! isotopic diffusion ratio

    real(r8) :: alpkn ! kinetic fractionation factor (1-kmol)

    !------------------------- Local Variables -----------------------------
    real(r8) :: reno                    ! surface reynolds number
    real(r8) :: tmr                     ! ratio of turbulent to molecular resistance
    real(r8) :: enn                     ! diffusive power
    real(r8) :: sc                      ! Schmidt number (Prandtl number)
    real(r8) :: vmu                     ! kinematic viscocity of air
    real(r8) :: difn                    ! ratio of difusivities to the power of n
    real(r8) :: kmol                    ! Merlivat's "k_mol"

    real(r8), parameter :: recrit   = 1.0_r8  ! critical Reynolds number for kmol
    !-----------------------------------------------------------------------

    vmu = muair / rbot          ! kinematic viscosity
    Sc  = vmu / difair          ! Schmidt number
    reno = ustar*zoq / vmu      ! Reynolds number

    if (reno < recrit) then ! Smooth regime
      enn = 2._r8/3._r8
      tmr  = ( (1._r8/karman)*log(ustar*zbot / (30._r8 * vmu)) ) / (13.6_r8 * Sc**(2._r8/3._r8))
    else                    ! Rough regime
      enn = 1._r8/2._r8
      tmr  = ( (1._r8/karman)*log(zbot/zoq) - 5._r8) / (7.3_r8 * reno**(1._r8/4._r8) * Sc**(1._r8/2._r8))
    end if

    difn = (1._r8/diff_ratio)**enn        ! use D/Di, not Di/D
    kmol = (difn - 1._r8) / (difn + tmr)

    alpkn = 1._r8 - kmol

  end function icam_atm_ocn_kinetic_frac_factor

!=========================================================================
end module shr_wiso_mod
