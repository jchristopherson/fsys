include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

set(fsys_test_sources
    fsys_test.f90
    fsys_test_routines.f90
)

add_executable(fsys_test ${fsys_test_sources})
link_library(fsys_test ${PROJECT_NAME} ${PROJECT_INCLUDE_DIR})
set_property(TARGET fsys_test PROPERTY LINKER_LANGUAGE Fortran)
add_test(
    NAME fsys_test
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:fsys_test>
)
