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

set(DIFF_SOURCE_FILES
    Differentiable.swift
    DifferentialOperators.swift
    DifferentiationUtilities.swift
    AnyDifferentiable.swift
    ArrayDifferentiation.swift
    OptionalDifferentiation.swift)

set(GYB_SOURCE_FILES
    FloatingPointDifferentiation.swift.gyb
    SIMDDifferentiation.swift.gyb)

# The logic handling gyb files gets confused if we want to pull the sources
# directly from ../Differentiation -- for now prefer copying those files
# in the current source directory
# A better solution would be to use symlinks, however
# those could not readily available under Windows
# (see https://github.com/git-for-windows/git/wiki/Symbolic-Links)
foreach(CURRENT_FILE IN LISTS DIFF_SOURCE_FILES GYB_SOURCE_FILES)
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${SOURCES_FOLDER}/${CURRENT_FILE}
    ${CMAKE_CURRENT_SOURCE_DIR} COPYONLY)
endforeach()

add_swift_target_library(swift_Differentiation ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  ${DIFF_SOURCE_FILES}

  GYB_SOURCES
    ${GYB_SOURCE_FILES}

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -parse-stdlib
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  DARWIN_INSTALL_NAME_DIR "@rpath"
  INSTALL_IN_COMPONENT stdlib)
