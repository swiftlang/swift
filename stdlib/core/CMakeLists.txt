#
# Determine SWIFTLIB_SOURCES, the list of source files in the swift library.
#

# This trick forces cmake to re-run whenever LibSources.txt
# changes.  We never use the copied result.
configure_file(LibSources.txt LibSources.txt.copy)

file(STRINGS LibSources.txt SWIFTLIB_SOURCES)

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
if (NOT CMAKE_SYSTEM_PROCESSOR STREQUAL i386)
  string(REPLACE ";Float80.swift" "" SWIFTLIB_SOURCES "${SWIFTLIB_SOURCES}")
endif()

# Strip comments and blank lines
string(REGEX REPLACE " *#[^;]*" "" SWIFTLIB_SOURCES "${SWIFTLIB_SOURCES}")
string(REGEX REPLACE ";;+" ";" SWIFTLIB_SOURCES "${SWIFTLIB_SOURCES}")
#
#
#

set(SHARED_LIBRARY ON)
add_swift_library(swift_stdlib INSTALL IS_STDLIB
  ${SWIFTLIB_SOURCES}
  DEPENDS swift_runtime)
add_dependencies(swift_stdlib swift)

if(SWIFT_OPTIMIZED)
  if (CMAKE_SYSTEM_PROCESSOR STREQUAL "i386")
   set_target_properties(swift_stdlib PROPERTIES
                                      COMPILE_FLAGS "-momit-leaf-frame-pointer")
  elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
   set_target_properties(swift_stdlib PROPERTIES
                                      COMPILE_FLAGS "-momit-leaf-frame-pointer")
 endif()
endif()

# Link against Foundation, for ObjC bridging.
set_target_properties(swift_stdlib PROPERTIES
                                    LINK_FLAGS "-framework Foundation")
add_dependencies(swift_stdlib swift)

# Create the symlinks necessary to get the compiler to find the
# swift runtime during development with Xcode.
add_custom_command(TARGET swift_stdlib PRE_BUILD
  COMMAND ln "-fhs" "${SWIFTLIB_DIR}" "${CMAKE_BINARY_DIR}/bin/lib"
  COMMAND mkdir "-p" "${SWIFTLIB_DIR}/swift"
  COMMAND ln "-fhs" "${SWIFT_PATH_TO_CLANG_BUILD}/lib/clang" "${SWIFTLIB_DIR}/swift/clang")

# Install Clang headers under the Swift library so that an installed swift's
# module importer can find the compiler headers corresponding to its clang.
install(
  DIRECTORY
    "${SWIFT_PATH_TO_CLANG_BUILD}/lib/clang"
  DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/swift"
  PATTERN "*.h")
