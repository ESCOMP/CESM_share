# Organization of the share source code

## Subdirectories

The source code is divided into the following subdirectories:

- **`const/`**: Kind parameters, physical constants and other shared constants.

- **`util/`**: General-purpose infrastructure that isn't tied to any particular science,
  such as error handling, logging, string and file utilities, and MPI and ESMF helpers.

- **`coupling/`**: Configuration of what is exchanged between components in a coupled
  run. Most of these modules read a namelist group from `drv_flds_in`.

- **`science/`**: Science calculations shared across components, such as
  atmosphere-ocean fluxes and orbital parameters.

- **`stubs/`**: Stub implementations of external libraries, used when the real library
  is not available. These stubs are not part of the dependency structure below.

## Dependency structure

The directories form a layered structure, with `const/` at the bottom, `util/` above it,
and `coupling/` and `science/` both above `util/`:

```
coupling     science
     \         /
      v       v
        util
          |
          v
        const
```

Each directory may depend on itself and on the directories below it:

- Code in `coupling/` and `science/` may use `util/` and `const/`.
- Code in `util/` may use only `const/`.
- Code in `const/` must not use any other directory.

The relationship between `coupling/` and `science/` is still to be determined. Currently,
neither depends on the other. It has not yet been decided whether `science/` should be
allowed to depend on `coupling/`, `coupling/` on `science/`, or neither, so please don't
add a dependency in either direction without discussing it first.

Please maintain this structure when adding or moving code. In particular, a module that
needs to be used by lower-level code belongs in a lower-level directory. A module
should never "reach up" to use a directory above it, because that could lead to
circular dependencies.
