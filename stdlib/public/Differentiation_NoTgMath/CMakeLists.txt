#===--- CMakeLists.txt - Differentiable programming support library ------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

set(SOURCES_FOLDER ../Differentiation)

add_swift_target_library(swift_Differentiation ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  ${SOURCES_FOLDER}/Differentiable.swift
  ${SOURCES_FOLDER}/DifferentialOperators.swift
  ${SOURCES_FOLDER}/DifferentiationUtilities.swift
  ${SOURCES_FOLDER}/AnyDifferentiable.swift
  ${SOURCES_FOLDER}/ArrayDifferentiation.swift
  ${SOURCES_FOLDER}/OptionalDifferentiation.swift

  GYB_SOURCES
    ${SOURCES_FOLDER}/FloatingPointDifferentiation.swift.gyb
    ${SOURCES_FOLDER}/SIMDDifferentiation.swift.gyb

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -parse-stdlib
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  DARWIN_INSTALL_NAME_DIR "@rpath"
  INSTALL_IN_COMPONENT stdlib)
