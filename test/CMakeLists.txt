include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

set(fsys_test_sources
    c_fsys_test.cpp
    string_tests.f90
    fsys_test_routines.f90
)

add_executable(fsys_test ${fsys_test_sources})
add_test(
    NAME fsys_test
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:fsys_test>
)
link_library(fsys_test ${PROJECT_NAME} ${PROJECT_INCLUDE_DIR})
target_include_directories(fsys_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
