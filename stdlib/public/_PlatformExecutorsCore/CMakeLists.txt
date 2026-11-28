#===--- CMakeLists.txt - Platform executors core library -------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===------------------------------------------------------------------------===#

# The protocols and vocabulary types of the I/O interfaces of the platform
# executors, such as `FileOperationScheduler`.
#
# This module must not depend on anything above `_Concurrency`.
# The sources are in the swift-platform-executors repository.
file(GLOB_RECURSE _PLATFORM_EXECUTORS_CORE_SOURCES CONFIGURE_DEPENDS
  "${SWIFT_PLATFORM_EXECUTORS_SOURCE_DIR}/Sources/_PlatformExecutorsCore/*.swift")
set(PLATFORM_EXECUTORS_CORE_SOURCES)
foreach(source ${_PLATFORM_EXECUTORS_CORE_SOURCES})
  file(TO_CMAKE_PATH "${source}" source)
  list(APPEND PLATFORM_EXECUTORS_CORE_SOURCES ${source})
endforeach()
message(STATUS "Using swift-platform-executors for _PlatformExecutorsCore (${SWIFT_PLATFORM_EXECUTORS_SOURCE_DIR}).")

# The Swift settings of the Package.swift of swift-platform-executors, with the
# `ExperimentalIO` trait enabled. `NonisolatedNonsendingByDefault` changes the
# ABI of the `async` requirements, so it must match the package build.
# The sources rely on the implicit import of `_Concurrency` for `async`, which
# add_swift_target_library disables, so import it explicitly.
set(swift_platform_executors_core_compile_flags
  -swift-version 6
  -enable-upcoming-feature ExistentialAny
  -enable-upcoming-feature MemberImportVisibility
  -enable-upcoming-feature InternalImportsByDefault
  -enable-upcoming-feature NonisolatedNonsendingByDefault
  -enable-experimental-feature Lifetimes
  -strict-memory-safety
  -D ExperimentalIO
  -Xfrontend -import-module -Xfrontend _Concurrency)

# The declarations use the `SwiftPlatformExecutors` availability macros, keyed
# by the version of swift-platform-executors. Define each of them with the
# availability of the `SwiftStdlib` macro that
# utils/platform-executors-availability-macros.def maps it to. They are added to
# the availability definitions of this directory, so add_swift_target_library
# handles them like the `SwiftStdlib` macros, for example for Embedded Swift.
set(platform_executors_availability_file
  "${SWIFT_SOURCE_DIR}/utils/platform-executors-availability-macros.def")
set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
  "${platform_executors_availability_file}")
file(STRINGS "${platform_executors_availability_file}"
  platform_executors_availability REGEX "^SwiftPlatformExecutors ")
set(platform_executors_availability_definitions)
foreach(mapping ${platform_executors_availability})
  # For example `SwiftPlatformExecutors 0.1.0:SwiftStdlib 6.5`.
  string(REGEX MATCH "^([^:]+):[ \t]*(.*[^ \t])[ \t]*$" _ "${mapping}")
  set(macro "${CMAKE_MATCH_1}")
  set(stdlib_macro "${CMAKE_MATCH_2}:")
  string(LENGTH "${stdlib_macro}" stdlib_macro_length)
  set(definition)
  foreach(stdlib_definition ${SWIFT_STDLIB_AVAILABILITY_DEFINITIONS})
    string(FIND "${stdlib_definition}" "${stdlib_macro}" position)
    if(position EQUAL 0)
      string(SUBSTRING "${stdlib_definition}" ${stdlib_macro_length} -1 availability)
      set(definition "${macro}:${availability}")
    endif()
  endforeach()
  if(NOT definition)
    message(FATAL_ERROR "'${mapping}' in ${platform_executors_availability_file} refers to a macro that is not in utils/availability-macros.def")
  endif()
  list(APPEND platform_executors_availability_definitions "${definition}")
endforeach()
list(APPEND SWIFT_STDLIB_AVAILABILITY_DEFINITIONS
  ${platform_executors_availability_definitions})

add_swift_target_library(swift_PlatformExecutorsCore ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  ${PLATFORM_EXECUTORS_CORE_SOURCES}

  SWIFT_MODULE_DEPENDS _Concurrency

  LINK_LIBRARIES swiftCore

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    ${swift_platform_executors_core_compile_flags}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

  INSTALL_IN_COMPONENT stdlib
  MACCATALYST_BUILD_FLAVOR "zippered"
)
