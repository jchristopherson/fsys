add_subdirectory(src)
add_library(cfsys ${CFSYS_SOURCES})
target_include_directories(cfsys PRIVATE include)
add_subdirectory(install)