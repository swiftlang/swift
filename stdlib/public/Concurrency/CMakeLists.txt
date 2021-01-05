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

set(swift_concurrency_objc_sources
  SwiftNativeNSObject.mm)

set(LLVM_OPTIONAL_SOURCES
  ${swift_concurrency_objc_sources})

set(swift_concurrency_link_libraries
  swiftCore)

if(SWIFT_CONCURRENCY_USES_DISPATCH)
  if(NOT CMAKE_SYSTEM_NAME STREQUAL Darwin)
    include_directories(AFTER
                          ${SWIFT_PATH_TO_LIBDISPATCH_SOURCE})

    # FIXME: we can't rely on libdispatch having been built for the
    # target at this point in the process.  Currently, we're relying
    # on soft-linking.
    #list(APPEND swift_concurrency_link_libraries
    #  dispatch)
  endif()
endif()


add_swift_target_library(swift_Concurrency ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  Actor.cpp
  Actor.swift
  GlobalExecutor.cpp
  PartialAsyncTask.swift
  Task.cpp
  Task.swift
  TaskCancellation.swift
  _TimeTypes.swift
  TaskAlloc.cpp
  TaskStatus.cpp
  TaskGroup.cpp
  TaskGroup.swift
  Mutex.cpp
  ${swift_concurrency_objc_sources}

  SWIFT_MODULE_DEPENDS_OSX Darwin
  SWIFT_MODULE_DEPENDS_IOS Darwin
  SWIFT_MODULE_DEPENDS_TVOS Darwin
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin
  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_OPENBSD Glibc
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
  SWIFT_MODULE_DEPENDS_HAIKU Glibc
  SWIFT_MODULE_DEPENDS_WINDOWS CRT

  LINK_LIBRARIES ${swift_concurrency_link_libraries}

  C_COMPILE_FLAGS
    -Dswift_Concurrency_EXPORTS
  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -parse-stdlib
    -Xfrontend -enable-experimental-concurrency
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  INSTALL_IN_COMPONENT stdlib
)
