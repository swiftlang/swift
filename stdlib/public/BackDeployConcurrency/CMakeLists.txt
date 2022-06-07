#===--- CMakeLists.txt - Back-deployed concurrency support library -------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

cmake_minimum_required(VERSION 3.19.6)

# This is always built standalone
include("${CMAKE_CURRENT_SOURCE_DIR}/../../../cmake/modules/StandaloneOverlay.cmake")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/modules")
set(SWIFT_STDLIB_STABLE_ABI TRUE)
set(SWIFT_STDLIB_SINGLE_THREADED_CONCURRENCY FALSE)
set(SWIFT_CONCURRENCY_GLOBAL_EXECUTOR "dispatch")
include(AddSwiftStdlib)

# Don't build the libraries for 32-bit iOS targets; there is no back-deployment
# to them.
list(REMOVE_ITEM SWIFT_SDK_IOS_ARCHITECTURES "armv7" "armv7s")
list(REMOVE_ITEM SWIFT_SDK_IOS_SIMULATOR_ARCHITECTURES "i386")

# Don't build the libraries for arm64e; it's not a stable ABI.
list(REMOVE_ITEM SWIFT_SDK_IOS_ARCHITECTURES "arm64e")
list(REMOVE_ITEM SWIFT_SDK_OSX_ARCHITECTURES "arm64e")

# The back-deployed library can only be shared.
list(APPEND SWIFT_STDLIB_LIBRARY_BUILD_TYPES SHARED)

# Link against the libswiftCore in the SDK. This intentionally avoids using
# the locally-built libswiftCore.
foreach(sdk ${SWIFT_SDKS})
    set(sdk_name ${SWIFT_SDK_${sdk}_LIB_SUBDIR})
    set(swift_core_target "swiftCore-${sdk_name}")
    add_library(${swift_core_target} SHARED IMPORTED GLOBAL)
    set_property(TARGET ${swift_core_target}
      PROPERTY IMPORTED_LOCATION "${SWIFT_SDK_${sdk}_PUBLIC_PATH}/usr/lib/swift/libswiftCore.tbd")
    foreach(arch in ${SWIFT_SDK_${sdk}_ARCHITECTURES})
      add_library("${swift_core_target}-${arch}" ALIAS "${swift_core_target}")
    endforeach()
endforeach()

# Build the concurrency library for back deployment.
add_compile_definitions(SWIFT_CONCURRENCY_BACK_DEPLOYMENT)
set(swift_concurrency_install_component back-deployment)
set(swift_concurrency_options
  BACK_DEPLOYMENT_LIBRARY 5.5
  DARWIN_INSTALL_NAME_DIR "@rpath"
  LINK_FLAGS -lobjc)
set(swift_concurrency_extra_sources
  "Exclusivity.cpp"
  "Metadata.cpp"
  "../stubs/SwiftNativeNSObject.mm")
set(swift_concurrency_async_fp_mode "never")

set(SWIFT_RUNTIME_CONCURRENCY_C_FLAGS)
set(SWIFT_RUNTIME_CONCURRENCY_SWIFT_FLAGS)

# Don't emit extended frame info on platforms other than darwin, system
# backtracer and system debugger are unlikely to support it.
list(APPEND SWIFT_RUNTIME_CONCURRENCY_C_FLAGS
  "-fswift-async-fp=${swift_concurrency_async_fp_mode}")
list(APPEND SWIFT_RUNTIME_CONCURRENCY_SWIFT_FLAGS
  "-Xfrontend"
  "-swift-async-frame-pointer=${swift_concurrency_async_fp_mode}")

list(APPEND SWIFT_RUNTIME_CONCURRENCY_C_FLAGS
  "-D__STDC_WANT_LIB_EXT1__=1")

add_swift_target_library(swift_Concurrency ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  CompatibilityOverride.cpp
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
  ConditionVariable.cpp
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
  ThreadingError.cpp
  AsyncStreamBuffer.swift
  AsyncStream.swift
  AsyncThrowingStream.swift
  AsyncStream.cpp
  Deque.swift
  ${swift_concurrency_extra_sources}
  ../Concurrency/linker-support/magic-symbols-for-install-name.c

  INCORPORATE_OBJECT_LIBRARIES_SHARED_ONLY
    swiftThreading
  LINK_LIBRARIES ${swift_concurrency_link_libraries}

  C_COMPILE_FLAGS
    -Dswift_Concurrency_EXPORTS ${SWIFT_RUNTIME_CONCURRENCY_C_FLAGS}
    -I${SWIFT_SOURCE_DIR}/stdlib/include
  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -parse-stdlib
    -Xfrontend -enable-experimental-concurrency
    ${SWIFT_RUNTIME_CONCURRENCY_SWIFT_FLAGS}
  ${swift_concurrency_options}
  INSTALL_IN_COMPONENT ${swift_concurrency_install_component}
)
