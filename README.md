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

## Building and running CESM_share unit tests

### Prerequisites

The unit test build requires [CIME](https://github.com/esmci/cime) for the sake of various
CMake modules. In addition, the easiest way to build and run the unit tests is to use
CIME's run_tests.py script, which leverages machine configuration information from
[ccs_config](https://github.com/esmci/ccs_config_cesm) or something similar.

If you are testing this share code in the context of a CESM (or CAM, CTSM, etc.) checkout,
then you already have the required cime and ccs_config_cesm repositories in the correct
location. If not, you will need to obtain recent versions of cime and ccs_config_cesm,
checking them out alongside this share code repository.

If this share code is at the path `/PATH/TO/CODE/share`, then do the following:

```
cd /PATH/TO/CODE
git clone https://github.com/ESMCI/cime.git
git clone https://github.com/ESMCI/ccs_config_cesm.git ccs_config
cd share
```

### Basic procedure to build and run the unit tests

You can build and run the unit tests with the following command; note that this reuses the
existing `unit_tests.temp` directory (if present) in order to do an incremental rebuild of
the unit tests from the last time you ran them from this directory:

```
../cime/scripts/fortran_unit_testing/run_tests.py --build-dir ./unit_tests.temp --cmake-args " -DUNITTESTS=ON -DUSE_CIME_MACROS=ON"
```
