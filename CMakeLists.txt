# Master CMAKE Build Script
cmake_minimum_required(VERSION 3.24)
project(
    fsys
    LANGUAGES Fortran C CXX
    VERSION 1.0.1
)

# Confgiure everything
add_subdirectory(configure)

# C/C++ Compiler Directions - Windows Specific
if (WIN32)
    # Necessary for COM support => REF: https://gist.github.com/armornick/106e1124995194af5c02
    set(CMAKE_CXX_FLAGS_DEBUG "-DUNICODE -D_UNICODE -D_WIN32_WINNT=_WIN32_WINNT_VISTA -DNTDDI_VERSION=NTDDI_VISTA -Wl,--allow-multiple-definition")
    set(CMAKE_CXX_FLAGS_RELEASE "-DUNICODE -D_UNICODE -D_WIN32_WINNT=_WIN32_WINNT_VISTA -DNTDDI_VERSION=NTDDI_VISTA -Wl,--allow-multiple-definition")
endif()

# Build
add_subdirectory(src)

# Include necessary headers
target_include_directories(${PROJECT_NAME} PRIVATE include)

# Define the OS flags for OS-specific code
if (WIN32)
    target_compile_definitions(${PROJECT_NAME} PRIVATE WIN32=1)
endif()

# Install
install(
  EXPORT ${PROJECT_NAME}-targets
  NAMESPACE ${PROJECT_NAME}::
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)

# Testing
option(BUILD_TESTING "Build tests")
include(CTest)
message(STATUS "Build FSYS tests: ${BUILD_TESTING}")
if (BUILD_TESTING)
    enable_testing()
    add_subdirectory(test)
endif()