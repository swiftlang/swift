#===--- CMakeLists.txt - Build the TensorFlow support library ------------===#
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

find_package(TensorFlow REQUIRED)
message(STATUS "Building TensorFlow.")

set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
set(swift_stdlib_compile_flags "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}")
list(APPEND swift_stdlib_compile_flags "-Xllvm" "-sil-inline-generics")
list(APPEND swift_stdlib_compile_flags "-Xllvm" "-sil-partial-specialization")
list(APPEND swift_stdlib_compile_flags "-force-single-frontend-invocation")
# FIXME(SR-7972): Some tests fail when TensorFlow is optimized.
# list(APPEND swift_stdlib_compile_flags "-O" "-whole-module-optimization")
list(APPEND swift_stdlib_compile_flags "-Onone")
list(APPEND swift_stdlib_compile_flags "-DCOMPILING_TENSORFLOW_STDLIB_MODULE")

set(SOURCES "")

# Copy TensorFlow high-level API sources, if they exist.
if (TENSORFLOW_SWIFT_APIS)
  message(STATUS "Using TensorFlow high-level APIs library.")

  file(GLOB_RECURSE TENSORFLOW_SWIFT_API_SOURCES
    "${TENSORFLOW_SWIFT_APIS}/Sources/*.swift")
  foreach(_TENSORFLOW_SWIFT_APIS_SOURCE ${TENSORFLOW_SWIFT_API_SOURCES})
    file(TO_CMAKE_PATH ${_TENSORFLOW_SWIFT_APIS_SOURCE} _TENSORFLOW_SWIFT_APIS_SOURCE)
    list(APPEND SOURCES ${_TENSORFLOW_SWIFT_APIS_SOURCE})
  endforeach()
endif()

add_swift_target_library(swiftTensorFlow ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  "${SOURCES}"

  INCORPORATE_OBJECT_LIBRARIES swiftCTensorFlow
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
  SWIFT_MODULE_DEPENDS_WINDOWS MSVCRT
  SWIFT_MODULE_DEPENDS Python
  SWIFT_COMPILE_FLAGS "${swift_stdlib_compile_flags}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  INSTALL_IN_COMPONENT stdlib
  EXTRA_RPATHS "${SWIFT_TENSORFLOW_TARGET_LIB_DIR}"
  # NOTE: This is a workaround for https://github.com/apple/swift/pull/24382,
  # which changed the default install_name_dir to `/usr/bin/swift`.
  DARWIN_INSTALL_NAME_DIR "@rpath")
