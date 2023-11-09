# Master CMAKE Build Script
cmake_minimum_required(VERSION 3.24)
project(
    fsys
    LANGUAGES Fortran C CXX
    VERSION 1.0.0
)

# Get helper macros and functions
include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

# Confgiure everything
add_subdirectory(configure)

# Build
add_subdirectory(src)
add_fortran_library(
    ${PROJECT_NAME}
    ${PROJECT_INCLUDE_DIR}
    ${CMAKE_INSTALL_INCLUDEDIR}
    ${PROJECT_VERSION}
    ${PROJECT_VERSION_MAJOR}
    ${FSYS_SOURCES}
)

# Include necessary headers
target_include_directories(${PROJECT_NAME} PRIVATE include)

# Installation
add_subdirectory(install)

# Testing
option(BUILD_TESTING "Build tests")
include(CTest)
message(STATUS "Build FSYS tests: ${BUILD_TESTING}")
if (BUILD_TESTING)
    enable_testing()
    add_subdirectory(test)
endif()