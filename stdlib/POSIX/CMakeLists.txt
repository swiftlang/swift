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
  DEPENDS swift_stdlib_core)

add_swift_optimization_flags(swift_stdlib_posix)
