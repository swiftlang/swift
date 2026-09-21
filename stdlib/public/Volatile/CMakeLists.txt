#===--- CMakeLists.txt ---------------------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2019 - 2022 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

add_swift_target_library(swift_Volatile ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  Volatile.swift

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -parse-stdlib
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

  INSTALL_IN_COMPONENT stdlib
)

if(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB)
  add_custom_target(embedded-volatile)
  add_dependencies(embedded-libraries embedded-volatile)

  add_embedded_swift_target_library(
    embedded-volatile
    swift_Volatile
    IS_SDK_OVERLAY

    Volatile.swift

    SWIFT_COMPILE_FLAGS
      -parse-stdlib
    INSTALL_IN_COMPONENT stdlib
  )
endif()
