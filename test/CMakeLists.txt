set(fsys_test_sources
    fsys_test.f90
    fsys_test_routines.f90
)

add_executable(fsys_test ${fsys_test_sources})
target_link_libraries(fsys_test fsys)
set_property(TARGET fsys_test PROPERTY LINKER_LANGUAGE Fortran)
add_test(
    NAME fsys_test
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:fsys_test>
)

# Copy over any DLLs
if (${BUILD_SHARED_LIBS} AND WIN32)
    add_custom_command(
        TARGET fsys_test
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy 
        $<TARGET_RUNTIME_DLLS:fsys_test> 
        $<TARGET_FILE_DIR:fsys_test>
        COMMAND_EXPAND_LISTS
    )
endif()
