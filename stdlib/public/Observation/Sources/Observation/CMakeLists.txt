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

# Linux and windows hosts are disabled for now; it is dependent upon earlyswiftsyntax
# which does not build in the same way just yet
if (${SWIFT_HOST_VARIANT_SDK} IN_LIST SWIFT_DARWIN_PLATFORMS)
  list(APPEND swift_runtime_library_compile_flags -I${SWIFT_SOURCE_DIR}/stdlib/include -I${SWIFT_SOURCE_DIR}/include)

  set(SWIFT_OBSERVATION_SWIFT_FLAGS)
  set(SWIFT_OBSERVATION_DEPENDS)

  list(APPEND SWIFT_OBSERVATION_SWIFT_FLAGS
    "-enable-experimental-feature" "Macros"
    "-DSWIFT_OBSERVATION_MACROS"
  )
  list(APPEND SWIFT_OBSERVATION_DEPENDS ObservationMacros)

  add_swift_target_library(swiftObservation ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
    Locking.cpp
    Locking.swift
    Macros.swift
    Observable.swift
    ObservationRegistrar.swift
    ObservationTracking.swift
    ObservedChanges.swift
    ObservedTransactions.swift
    ThreadLocal.cpp
    ThreadLocal.swift
    TrackedProperties.swift

    DEPENDS ${SWIFT_OBSERVATION_DEPENDS}

    SWIFT_COMPILE_FLAGS
      ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
      ${SWIFT_OBSERVATION_SWIFT_FLAGS}
    SWIFT_MODULE_DEPENDS _Concurrency
    INSTALL_IN_COMPONENT stdlib

    MACCATALYST_BUILD_FLAVOR "zippered"
  )

endif()