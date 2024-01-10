# fsys
FSYS is a Fortran library containing system operations and supporting types.

## Status
![Build Status](https://github.com/jchristopherson/fsys/actions/workflows/cmake.yml/badge.svg)
[![Actions Status](https://github.com/jchristopherson/fsys/workflows/fpm/badge.svg)](https://github.com/jchristopherson/fsys/actions)

## Documentation
Documentation can be found [here](https://jchristopherson.github.io/fsys/)

## Building FSYS
[CMake](https://cmake.org/) can be used to build this library.  Use -DBUILD_TESTING=TRUE only if tests are desired.  If tests are not to be built, then simply omit -DBUILD_TESTING.  The default is a release build static library.
```txt
cd build
cmake ../build -DBUILD_TESTING=TRUE
make
```
For more detailed instructions see [Running CMake](https://cmake.org/runningcmake/).

[FPM](https://github.com/fortran-lang/fpm) can also be used to build this library using the provided fpm.toml.
```txt
fpm build
```
The FSYS library can be used within your FPM project by adding the following to your fpm.toml file.
```toml
[dependencies]
fsys = { git = "https://github.com/jchristopherson/fsys" }
```

## Dependencies
The FSYS library depends upon the following libraries.
- [FSTRING](https://github.com/jchristopherson/fstring)