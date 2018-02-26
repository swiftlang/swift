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

if(SWIFT_ENABLE_TENSORFLOW)
  link_directories(${SWIFT_TENSORFLOW_PATH_FOR_TARGET}/lib)
  set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)

  set(GROUP_INFO_JSON_FILE ${CMAKE_CURRENT_SOURCE_DIR}/GroupInfo.json)
  set(swift_stdlib_compile_flags "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}")

  list(APPEND swift_stdlib_compile_flags "-Xllvm" "-sil-inline-generics")
  list(APPEND swift_stdlib_compile_flags "-Xllvm" "-sil-partial-specialization")
  list(APPEND swift_stdlib_compile_flags "-Xfrontend" "-enable-sil-ownership")
  list(APPEND swift_stdlib_compile_flags "-swift-version" "4")
  list(APPEND swift_stdlib_compile_flags "-force-single-frontend-invocation")
  list(APPEND swift_stdlib_compile_flags "-Onone")

  add_swift_library(swiftTensorFlow ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
      ADRuntime.swift
      CompilerRuntime.swift
      CompositeMath.swift
      DataTypes.swift
      Gradients.swift
      Ops.swift
      RankedArray.swift.gyb
      RankedTensor.swift.gyb
      ShapedArray.swift
      Tensor.swift
      TensorHandle.swift
      TensorProtocol.swift
      TensorShape.swift
      Utilities.swift
      INCORPORATE_OBJECT_LIBRARIES swiftTensorFlowInit
      PRIVATE_LINK_LIBRARIES "tensorflow"
      SWIFT_MODULE_DEPENDS SwiftOnoneSupport
      SWIFT_MODULE_DEPENDS_LINUX Glibc
      SWIFT_COMPILE_FLAGS "${swift_stdlib_compile_flags}"
      LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
      INSTALL_IN_COMPONENT stdlib
      EXTRA_RPATH "${SWIFT_TENSORFLOW_PATH_FOR_TARGET}/lib")
endif()
