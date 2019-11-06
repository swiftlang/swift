#===--- CMakeLists.txt - Differentiable programming support library ------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

# SWIFT_ENABLE_TENSORFLOW
# NOTE: A non-empty `_Differentiation` module is currently created only on
# master branch, not on tensorflow branch.
#
# Instead, on tensorflow branch, the differentiation-related Swift source files
# in this directory are directly built as part of swiftCore: see
# stdlib/public/core/CMakeLists.txt. The `_Differentiation` module is created
# empty to avoid `#if canImport(_Differentiation)` guards in tests.
# SWIFT_ENABLE_TENSORFLOW END

add_swift_target_library(swift_Differentiation ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  Empty.swift

  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  INSTALL_IN_COMPONENT stdlib)
