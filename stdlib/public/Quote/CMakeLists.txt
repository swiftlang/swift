#===-------------- CMakeLists.txt - Build the Quote library ---------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#
#
# SWIFT_ENABLE_TENSORFLOW
#
#===----------------------------------------------------------------------===#

if(NOT SWIFT_ENABLE_TENSORFLOW)
  return()
endif()

set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
set(swift_stdlib_compile_flags "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}")

set(SOURCES "")

# Copy Quote sources, if they exist.
if (TENSORFLOW_SWIFT_QUOTE)
  file(GLOB_RECURSE TENSORFLOW_SWIFT_QUOTE_SOURCES
    "${TENSORFLOW_SWIFT_QUOTE}/Sources/Quote/*.swift")
  list(APPEND SOURCES "${TENSORFLOW_SWIFT_QUOTE_SOURCES}")
endif()

add_swift_target_library(swiftQuote ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  "${SOURCES}"

  TARGET_SDKS OSX LINUX
  PRIVATE_LINK_LIBRARIES "${TF_LIBRARIES}"
  SWIFT_MODULE_DEPENDS SwiftOnoneSupport
  SWIFT_MODULE_DEPENDS_IOS Darwin
  SWIFT_MODULE_DEPENDS_OSX Darwin
  SWIFT_MODULE_DEPENDS_TVOS Darwin
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin
  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
  SWIFT_MODULE_DEPENDS_HAIKU Glibc
  SWIFT_COMPILE_FLAGS "${swift_stdlib_compile_flags}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  INSTALL_IN_COMPONENT stdlib
  # NOTE: This is a workaround for https://github.com/apple/swift/pull/24382,
  # which changed the default install_name_dir to `/usr/bin/swift`.
  DARWIN_INSTALL_NAME_DIR "@rpath")
