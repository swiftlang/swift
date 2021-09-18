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

if(NOT swift_concurrency_extra_sources)
  set(swift_concurrency_extra_sources)
endif()

if(NOT swift_concurrency_install_component)
  set(swift_concurrency_install_component stdlib)
endif()

if(SWIFT_CONCURRENCY_USES_DISPATCH)
  if(NOT CMAKE_SYSTEM_NAME STREQUAL Darwin)
    include_directories(AFTER
                          ${SWIFT_PATH_TO_LIBDISPATCH_SOURCE})

    # FIXME: we can't rely on libdispatch having been built for the
    # target at this point in the process.  Currently, we're relying
    # on soft-linking.
    list(APPEND swift_concurrency_link_libraries
      dispatch)
  endif()
endif()


set(SWIFT_RUNTIME_CONCURRENCY_C_FLAGS)
set(SWIFT_RUNTIME_CONCURRENCY_SWIFT_FLAGS)

if(NOT swift_concurrency_async_fp_mode)
  set(swift_concurrency_async_fp_mode "always")
endif()

# Don't emit extended frame info on platforms other than darwin, system
# backtracer and system debugger are unlikely to support it.
if(CMAKE_SYSTEM_NAME STREQUAL Darwin)
  list(APPEND SWIFT_RUNTIME_CONCURRENCY_C_FLAGS
    "-fswift-async-fp=${swift_concurrency_async_fp_mode}")
  list(APPEND SWIFT_RUNTIME_CONCURRENCY_SWIFT_FLAGS
    "-Xfrontend"
    "-swift-async-frame-pointer=${swift_concurrency_async_fp_mode}")
else()
  list(APPEND SWIFT_RUNTIME_CONCURRENCY_C_FLAGS "-fswift-async-fp=never")
endif()

add_swift_target_library(swift_Concurrency ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  ../CompatibilityOverride/CompatibilityOverride.cpp
  Actor.cpp
  Actor.swift
  AsyncLet.cpp
  AsyncLet.swift
  CheckedContinuation.swift
  GlobalExecutor.cpp
  Errors.swift
  Error.cpp
  Executor.swift
  AsyncCompactMapSequence.swift
  AsyncDropFirstSequence.swift
  AsyncDropWhileSequence.swift
  AsyncFilterSequence.swift
  AsyncFlatMapSequence.swift
  AsyncIteratorProtocol.swift
  AsyncMapSequence.swift
  AsyncPrefixSequence.swift
  AsyncPrefixWhileSequence.swift
  AsyncSequence.swift
  AsyncThrowingCompactMapSequence.swift
  AsyncThrowingDropWhileSequence.swift
  AsyncThrowingFilterSequence.swift
  AsyncThrowingFlatMapSequence.swift
  AsyncThrowingMapSequence.swift
  AsyncThrowingPrefixWhileSequence.swift
  GlobalActor.swift
  MainActor.swift
  PartialAsyncTask.swift
  SourceCompatibilityShims.swift
  Task.cpp
  Task.swift
  TaskCancellation.swift
  TaskAlloc.cpp
  TaskStatus.cpp
  TaskGroup.cpp
  TaskGroup.swift
  TaskLocal.cpp
  TaskLocal.swift
  TaskSleep.swift
  ThreadSanitizer.cpp
  Mutex.cpp
  AsyncStreamBuffer.swift
  AsyncStream.swift
  AsyncThrowingStream.swift
  AsyncStream.cpp
  Deque.swift
  ${swift_concurrency_extra_sources}

  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_OPENBSD Glibc
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
  SWIFT_MODULE_DEPENDS_HAIKU Glibc
  SWIFT_MODULE_DEPENDS_WINDOWS CRT

  LINK_LIBRARIES ${swift_concurrency_link_libraries}

  C_COMPILE_FLAGS
    -Dswift_Concurrency_EXPORTS ${SWIFT_RUNTIME_CONCURRENCY_C_FLAGS}
  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -parse-stdlib
    -Xfrontend -enable-experimental-concurrency
    -Xfrontend -define-availability
    -Xfrontend "SwiftStdlib 5.5:macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0"
    ${SWIFT_RUNTIME_CONCURRENCY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_CONCURRENCY_SWIFT_LINK_FLAGS}"
  ${swift_concurrency_options}
  INSTALL_IN_COMPONENT ${swift_concurrency_install_component}
)
