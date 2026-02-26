set(dir ${CMAKE_CURRENT_SOURCE_DIR})

set(FSYS_SOURCES
    ${dir}/fsys.f90
    ${dir}/fsys.cpp
)

# Dependencies
find_package(fstring QUIET)

if (NOT fstring_FOUND)
    include(FetchContent)
    message(STATUS "FSTRING could not be found.  A reference version will be employed.")
    FetchContent_Declare(
        fstring
        GIT_REPOSITORY "https://github.com/jchristopherson/fstring"
        GIT_TAG main
    )
    FetchContent_MakeAvailable(fstring)
    set(FSTRING_LIBRARIES fstring)
    set(BUILD_FSTRING TRUE)
else()
    set(FSTRING_LIBRARIES fstring::fstring)
    set(BUILD_FSTRING FALSE)
endif()

# Build
add_library(${PROJECT_NAME} ${FSYS_SOURCES})
target_link_libraries(
    ${PROJECT_NAME}
    PUBLIC
    ${FSTRING_LIBRARIES}
)

set_target_properties(
    ${PROJECT_NAME}
    PROPERTIES
    POSITION_INDEPENDENT_CODE ON
    WINDOWS_EXPORT_ALL_SYMBOLS ON
)

set(FSYS_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod_files/)
if (NOT EXISTS "${FSYS_MOD_DIR}")
    file(MAKE_DIRECTORY "${FSYS_MOD_DIR}")
endif()

set_target_properties(
    ${PROJECT_NAME} PROPERTIES
    Fortran_MODULE_DIRECTORY ${FSYS_MOD_DIR}
)
target_include_directories(
    ${PROJECT_NAME} PUBLIC
    $<BUILD_INTERFACE:${FSYS_MOD_DIR}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_MODULEDIR}>
)

# Install
install(
    TARGETS ${PROJECT_NAME}
    EXPORT ${PROJECT_NAME}-targets
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
install(DIRECTORY ${FSYS_MOD_DIR} DESTINATION ${CMAKE_INSTALL_MODULEDIR})

if (${BUILD_FSTRING} AND ${BUILD_SHARED_LIBS})
    install(IMPORTED_RUNTIME_ARTIFACTS ${FSTRING_LIBRARIES})
endif()
