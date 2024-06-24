# fsys
FSYS is a Fortran library containing system operations and supporting types.

## Status
![Build Status](https://github.com/jchristopherson/fsys/actions/workflows/cmake.yml/badge.svg)

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

## Dependencies
The FSYS library depends upon the following libraries.
- [FSTRING](https://github.com/jchristopherson/fstring)