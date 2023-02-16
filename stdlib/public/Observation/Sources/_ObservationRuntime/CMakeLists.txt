#===--- CMakeLists.txt - Observation support library ---------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2023 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

set(SWIFT_OBSERVATION_SWIFT_FLAGS)

list(APPEND SWIFT_OBSERVATION_SWIFT_FLAGS
  "-parse-stdlib")

add_swift_target_library(swift_ObservationRuntime ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  Locking.cpp
  Locking.swift
  ThreadLocal.cpp
  ThreadLocal.swift

  C_COMPILE_FLAGS
    -I${SWIFT_SOURCE_DIR}/stdlib/include
  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    ${SWIFT_OBSERVATION_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT stdlib)

