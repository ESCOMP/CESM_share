# CESM_share

## Description of CESM_share

This module exists to collect code shared between various CESM components.
Excluding this "shared code" module, CESM components are built using disjoint
sets of source code.  The use of this shared code is similar to the use of
object code libraries where each subdirectory of share is equivalant to
one library.  While object library routines are accessed by linking to libraries
during the load phase, these shared code routines are accessed by including the
appropriate source code directory path during the compile phase.

Motivation for this code sharing includes:

- facilitating consistent physics between all models.  For example, uniform
  solar-zenith-angle/orbital calculations and uniform physical constants.
- providing an interface/API between component models and the flux-coupler
  component in the CESM framework.
- avoiding the need for redundant implementations of commonly needed
  functionality.  For example netCDF file reading, basic mapping (re-gridding)
  functionality, and common character string manipulations.

Current subsets ("libraries") of shared code only include:

util - very generic, general-purpose code that is likely to be useful to all
      CESM components.  CESM components may be explicitly required to use some
      parts of this code, for example the physical constants module.

