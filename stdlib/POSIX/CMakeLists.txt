#
# Determine POSIXLIB_SOURCES, the list of source files in the swift library.
#

# Ensure that cmake is re-run whenever POSIXLibSources.txt changes.
configure_file(POSIXLibSources.txt POSIXLibSources.txt.copy)

file(STRINGS POSIXLibSources.txt POSIXLIB_SOURCES)

# Strip comments and blank lines
string(REGEX REPLACE " *#[^;]*" "" POSIXLIB_SOURCES "${POSIXLIB_SOURCES}")
string(REGEX REPLACE ";;+" ";" POSIXLIB_SOURCES "${POSIXLIB_SOURCES}")


set(SHARED_LIBRARY ON)
add_swift_library(swift_stdlib_posix INSTALL
  ${POSIXLIB_SOURCES}
  DEPENDS swift_runtime swift_stdlib)
# Link against Foundation, for ObjC bridging.
set_target_properties(swift_stdlib_posix PROPERTIES
                                         LINK_FLAGS "-framework Foundation")
add_dependencies(swift_stdlib_posix swift_stdlib)

if(SWIFT_OPTIMIZED)
  if (CMAKE_SYSTEM_PROCESSOR STREQUAL "i386")
   set_target_properties(swift_stdlib_posix PROPERTIES
                                            COMPILE_FLAGS "-momit-leaf-frame-pointer")
  elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
   set_target_properties(swift_stdlib_posix PROPERTIES
                                            COMPILE_FLAGS "-momit-leaf-frame-pointer")
 endif()
endif()

