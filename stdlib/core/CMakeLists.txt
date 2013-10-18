#
# Determine SWIFTLIB_SOURCES, the list of source files in the swift library.
#

# This trick forces cmake to re-run whenever SwiftLibSources.txt
# changes.  We never use the copied result.
configure_file(SwiftLibSources.txt SwiftLibSources.txt.copy)

file(STRINGS SwiftLibSources.txt SWIFTLIB_SOURCES)

# Replace Word.swift with Word64.swift or Word32.swift, according to
# the target architecture
if(CMAKE_SIZEOF_VOID_P EQUAL 8)
  set(word_size 64)
elseif(CMAKE_SIZEOF_VOID_P EQUAL 4)
  set(word_size 32)
else()
  message(SEND_ERROR
    "Target word size of ${CMAKE_SIZEOF_VOID_P} bytes not accounted for with an appropriate WordXX.swift")
endif()

string(REPLACE ";Word.swift" ";Word${word_size}.swift" SWIFTLIB_SOURCES "${SWIFTLIB_SOURCES}")

# Replace Assert.swift with AssertDebug.swift if assertions are enabled
if(SWIFT_ASSERTS)
  string(REPLACE ";Assert.swift" ";AssertDebug.swift" SWIFTLIB_SOURCES "${SWIFTLIB_SOURCES}")
endif()

# Remove Float80.swift on non-x86 architectures
if (NOT CMAKE_SYSTEM_PROCESSOR STREQUAL i386 AND
    NOT CMAKE_SYSTEM_PROCESSOR STREQUAL x86_64)
  string(REPLACE ";Float80.swift" "" SWIFTLIB_SOURCES "${SWIFTLIB_SOURCES}")
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
add_dependencies(swift_stdlib_core swift)

# Create some output directories.
add_custom_command(TARGET swift_stdlib_core PRE_BUILD
  COMMAND mkdir "-p" "${SWIFTLIB_DIR}/swift"
  COMMAND mkdir "-p" "${SWIFTLIB_DIR}/axle"
)


# The stdlib will be cut down further than what we have included here.
configure_file(AxleSwiftLibSources.txt AxleSwiftLibSources.txt.copy)
file(STRINGS AxleSwiftLibSources.txt AXLE_SWIFTLIB_SOURCES)
string(REPLACE ";Word.swift" ";Word${word_size}.swift" AXLE_SWIFTLIB_SOURCES "${AXLE_SWIFTLIB_SOURCES}")
if(SWIFT_ASSERTS)
  string(REPLACE ";Assert.swift" ";AssertDebug.swift" AXLE_SWIFTLIB_SOURCES "${AXLE_SWIFTLIB_SOURCES}")
endif()
# Strip comments and blank lines
string(REGEX REPLACE " *#[^;]*" "" AXLE_SWIFTLIB_SOURCES "${AXLE_SWIFTLIB_SOURCES}")
string(REGEX REPLACE ";;+" ";" AXLE_SWIFTLIB_SOURCES "${AXLE_SWIFTLIB_SOURCES}")

add_swift_library(swift_stdlib_axle_core INSTALL AXLE IS_STDLIB_CORE
  ${AXLE_SWIFTLIB_SOURCES}
  DEPENDS swift_runtime)
add_dependencies(swift_stdlib_axle_core swift)
add_swift_optimization_flags(swift_stdlib_axle_core)
set_target_properties(swift_stdlib_axle_core PROPERTIES
                                             LINK_FLAGS "-all_load -Xlinker -reexport-lobjc")
add_dependencies(swift_stdlib_axle_core swift_stdlib_core)


# Install Clang headers under the Swift library so that an installed swift's
# module importer can find the compiler headers corresponding to its clang.
install(
  DIRECTORY
    "${SWIFT_PATH_TO_CLANG_BUILD}/lib/clang"
  DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swift"
  PATTERN "*.h")

