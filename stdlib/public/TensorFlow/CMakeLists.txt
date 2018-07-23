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

include_directories(BEFORE "${TF_INCLUDE_DIR}")

set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
set(swift_stdlib_compile_flags "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}")
list(APPEND swift_stdlib_compile_flags "-Xllvm" "-sil-inline-generics")
list(APPEND swift_stdlib_compile_flags "-Xllvm" "-sil-partial-specialization")
list(APPEND swift_stdlib_compile_flags "-Xfrontend" "-enable-sil-ownership")
list(APPEND swift_stdlib_compile_flags "-swift-version" "4")
list(APPEND swift_stdlib_compile_flags "-force-single-frontend-invocation")
list(APPEND swift_stdlib_compile_flags "-Xcc" "-I${TF_INCLUDE_DIR}")
# FIXME(SR-7972): Some tests fail when TensorFlow is optimized.
list(APPEND swift_stdlib_compile_flags "-Onone")

set(SOURCES
  CompilerRuntime.swift
  CompositeMath.swift
  DataTypes.swift
  Gradients.swift
  OpaqueHandles.swift
  Ops.swift
  Parameterized.swift
  ShapedArray.swift
  Tensor.swift
  TensorHandle.swift
  TensorProtocol.swift
  TensorShape.swift
  Utilities.swift)

# Copy TensorFlow bindings file, if it exists.
if (TENSORFLOW_SWIFT_BINDINGS)
  list(APPEND SOURCES "${TENSORFLOW_SWIFT_BINDINGS}")
endif()

add_swift_library(swiftTensorFlow ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  "${SOURCES}"

  INCORPORATE_OBJECT_LIBRARIES swiftCTensorFlow
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
  EXTRA_RPATH "${SWIFT_TENSORFLOW_TARGET_LIB_DIR}")
