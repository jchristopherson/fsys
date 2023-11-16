set(dir ${CMAKE_CURRENT_SOURCE_DIR})

set(FSYS_SOURCES
    ${dir}/fsys.f90
    ${dir}/strings.f90
    ${dir}/regular_expressions.cpp
    ${dir}/fsys.cpp
)
set(FSYS_SOURCES ${FSYS_SOURCES} PARENT_SCOPE)