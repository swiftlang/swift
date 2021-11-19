#===--- CMakeLists.txt - String processing support library -----------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===------------------------------------------------------------------------===#

set(swift_string_processing_link_libraries
  swiftCore
  swift_MatchingEngine)


add_swift_target_library(swift_StringProcessing ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  Regex.swift

  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_OPENBSD Glibc
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
  SWIFT_MODULE_DEPENDS_HAIKU Glibc
  SWIFT_MODULE_DEPENDS_WINDOWS CRT

  LINK_LIBRARIES ${swift_string_processing_link_libraries}

  C_COMPILE_FLAGS
    -Dswift_StringProcessing_EXPORTS
  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -parse-stdlib
    -Xfrontend -enable-experimental-string-processing
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

  SWIFT_MODULE_DEPENDS _MatchingEngine
  INSTALL_IN_COMPONENT stdlib
)
