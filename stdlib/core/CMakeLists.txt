#
# Determine SWIFTLIB_SOURCES, the list of source files in the swift library.
#

# This trick forces cmake to re-run whenever SwiftLibSources.txt
# changes.  We never use the copied result.
configure_file(SwiftLibSources.txt SwiftLibSources.txt.copy)

file(STRINGS SwiftLibSources.txt SWIFTLIB_SOURCES)

# Replace Assert.swift with AssertDebug.swift if assertions are enabled
if(SWIFT_ASSERTS)
  string(REPLACE ";Assert.swift" ";AssertDebug.swift" SWIFTLIB_SOURCES "${SWIFTLIB_SOURCES}")
endif()

if(NOT SWIFT_ENABLE_BENCHMARKS)
  string(REPLACE ";Benchmark.swift" "" SWIFTLIB_SOURCES "${SWIFTLIB_SOURCES}")
endif()

# Strip comments and blank lines
string(REGEX REPLACE " *#[^;]*" "" SWIFTLIB_SOURCES "${SWIFTLIB_SOURCES}")
string(REGEX REPLACE ";;+" ";" SWIFTLIB_SOURCES "${SWIFTLIB_SOURCES}")


set(SHARED_LIBRARY ON)
add_swift_library(swift_stdlib_core INSTALL IS_STDLIB_CORE
  ${SWIFTLIB_SOURCES}
  DEPENDS swift_runtime)
add_dependencies(swift_stdlib_core swift)

add_swift_optimization_flags(swift_stdlib_core)

# Link against Foundation, for ObjC bridging.
set_target_properties(swift_stdlib_core PROPERTIES
                                        LINK_FLAGS "-all_load -Xlinker -reexport-lobjc")

# Create some output directories and symlink in the Clang headers.
add_custom_command(TARGET swift_stdlib_core PRE_BUILD
  COMMAND mkdir "-p" "${SWIFTLIB_DIR}/${SWIFTLIB_SUBDIR}"
  COMMAND ln "-fns"
    "${SWIFT_PATH_TO_CLANG_BUILD}/${CMAKE_CFG_INTDIR}/lib/clang"
    "${SWIFTLIB_DIR}/clang")

# Install Clang headers under the Swift library so that an installed swift's
# module importer can find the compiler headers corresponding to its clang.
install(
  DIRECTORY
    "${SWIFT_PATH_TO_CLANG_BUILD}/lib/clang"
  DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swift"
  PATTERN "*.h")

