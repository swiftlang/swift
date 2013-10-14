#
# Determine AXLELIB_SOURCES, the list of source files in the swift library.
#

# Ensure that cmake is re-run whenever AXLELibSources.txt changes.
configure_file(SwiftAxleSources.txt SwiftAxleSources.txt.copy)

file(STRINGS SwiftAxleSources.txt AXLELIB_SOURCES)

# Remove Vec2h.swift for all architectures that don't support half.
if (NOT CMAKE_SYSTEM_PROCESSOR STREQUAL air)
  string(REPLACE ";Vec2h.swift" "" AXLELIB_SOURCES "${AXLELIB_SOURCES}")
endif()


# Strip comments and blank lines
string(REGEX REPLACE " *#[^;]*" "" AXLELIB_SOURCES "${AXLELIB_SOURCES}")
string(REGEX REPLACE ";;+" ";" AXLELIB_SOURCES "${AXLELIB_SOURCES}")


set(SHARED_LIBRARY ON)
add_swift_library(swift_stdlib_axle INSTALL AXLE
  ${AXLELIB_SOURCES}
  DEPENDS swift_stdlib_core)

add_swift_optimization_flags(swift_stdlib_axle)
