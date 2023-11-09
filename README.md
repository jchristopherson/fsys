# fsys
FSYS is a Fortran library containing system operations and supporting types.

## Status
![Build Status](https://github.com/jchristopherson/fsys/actions/workflows/cmake.yml/badge.svg)
[![Actions Status](https://github.com/jchristopherson/fsys/workflows/fpm/badge.svg)](https://github.com/jchristopherson/fsys/actions)

## Types
- string: This is a string type similar to the [iso_varying_string](https://gitlab.com/everythingfunctional/iso_varying_string), but with a few differences and a few additional operations.  There is also support for regular expressions.
- string_builder: This is a type that allows concatenating strings while minimizing memory reallocation operations.  This type behaves similarily to the .NET StringBuilder class.

## Documentation
Documentation can be found [here](https://jchristopherson.github.io/fsys/)

## Building FSYS
[CMake](https://cmake.org/)This library can be built using CMake.  For instructions see [Running CMake](https://cmake.org/runningcmake/).

[FPM](https://github.com/fortran-lang/fpm) can also be used to build this library using the provided fpm.toml.
```txt
fpm build
```
The FSYS library can be used within your FPM project by adding the following to your fpm.toml file.
```toml
[dependencies]
fsys = { git = "https://github.com/jchristopherson/fsys" }
```