#===--- CMakeLists.txt - Concurrency support library ---------------------===#
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

set(swift_distributed_link_libraries
  swiftCore)

add_swift_target_library(swift_Distributed ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  AssertDistributed.swift
  DistributedActor.swift

  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_OPENBSD Glibc
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
  SWIFT_MODULE_DEPENDS_HAIKU Glibc
  SWIFT_MODULE_DEPENDS_WINDOWS CRT

  LINK_LIBRARIES ${swift_distributed_link_libraries}

  C_COMPILE_FLAGS
    -Dswift_Distributed_EXPORTS
  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -parse-stdlib
    -Xfrontend -enable-experimental-distributed
    -Xfrontend -define-availability
    -Xfrontend \"SwiftStdlib 5.5:macOS 9999, iOS 9999, watchOS 9999, tvOS 9999\"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

  SWIFT_MODULE_DEPENDS _Concurrency
  INSTALL_IN_COMPONENT stdlib
)
