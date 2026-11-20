#===--- CMakeLists.txt - Platform executors collections library ------------===#
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

# The containers of swift-collections that `PlatformExecutors` uses, such as
# `UniqueArray` and `UniqueDeque`, built as one private module.
#
# The module has its own name, so its symbols can't clash with a copy of
# swift-collections in the same process, and only `PlatformExecutors` may
# import it. `PlatformExecutors` imports it with `internal import`, so it
# doesn't appear in the interface of `PlatformExecutors`. Foundation does the
# same with `_FoundationCollections`.
#
# The sources are in the swift-collections repository, which provides the
# `COLLECTIONS_SINGLE_MODULE` condition for building several of its modules as
# one.
set(_PLATFORM_EXECUTORS_COLLECTIONS_MODULES
  InternalCollectionsUtilities
  SpanPreview
  BasicContainers
  DequeModule)
set(_PLATFORM_EXECUTORS_COLLECTIONS_GLOBS)
foreach(module ${_PLATFORM_EXECUTORS_COLLECTIONS_MODULES})
  list(APPEND _PLATFORM_EXECUTORS_COLLECTIONS_GLOBS
    "${SWIFT_PATH_TO_SWIFT_COLLECTIONS_SOURCE}/Sources/${module}/*.swift")
endforeach()
file(GLOB_RECURSE _PLATFORM_EXECUTORS_COLLECTIONS_SOURCES CONFIGURE_DEPENDS
  ${_PLATFORM_EXECUTORS_COLLECTIONS_GLOBS})
# The stdlib build passes the sources to the compiler in a response file, which
# the compiler splits at spaces, and only takes a file with a single dot in its
# name for a Swift source. Some swift-collections sources have spaces
# (`Integer rank.swift`) or more dots (`Deque._Storage.swift`) in their names,
# so build copies of those with plain names instead. The file names don't
# matter for the module.
get_filename_component(_platform_executors_collections_sources_dir
  "${SWIFT_PATH_TO_SWIFT_COLLECTIONS_SOURCE}/Sources" ABSOLUTE)
set(PLATFORM_EXECUTORS_COLLECTIONS_SOURCES)
foreach(source ${_PLATFORM_EXECUTORS_COLLECTIONS_SOURCES})
  get_filename_component(source "${source}" ABSOLUTE)
  get_filename_component(name "${source}" NAME_WLE)
  if(name MATCHES "[ .]")
    file(RELATIVE_PATH relative_path
      "${_platform_executors_collections_sources_dir}" "${source}")
    get_filename_component(relative_dir "${relative_path}" DIRECTORY)
    string(REGEX REPLACE "[ .]" "_" plain_name "${name}")
    set(copy "${CMAKE_CURRENT_BINARY_DIR}/Sources/${relative_dir}/${plain_name}.swift")
    configure_file("${source}" "${copy}" COPYONLY)
    set(source "${copy}")
  endif()
  file(TO_CMAKE_PATH "${source}" source)
  list(APPEND PLATFORM_EXECUTORS_COLLECTIONS_SOURCES "${source}")
endforeach()
message(STATUS "Using swift-collections for _PlatformExecutorsCollections (${SWIFT_PATH_TO_SWIFT_COLLECTIONS_SOURCE}).")

# The Swift settings of the Package.swift of swift-collections, with the
# `UnstableHashedContainers` trait enabled for `RigidDictionary` and
# `UniqueDictionary`.
set(swift_platform_executors_collections_compile_flags
  -swift-version 6
  -package-name swift-collections
  -D COLLECTIONS_SINGLE_MODULE
  -D UnstableHashedContainers
  -enable-upcoming-feature MemberImportVisibility
  -enable-experimental-feature BuiltinModule
  -enable-experimental-feature Lifetimes
  -enable-experimental-feature InoutLifetimeDependence
  -enable-experimental-feature SuppressedAssociatedTypesWithDefaults
  -strict-memory-safety
  -Xfrontend -allowable-client -Xfrontend PlatformExecutors)

# swift-collections isn't built with library evolution, and its sources don't
# support it: for example, they use `@_alwaysEmitIntoClient` on stored
# properties of structs that aren't `@frozen`. The module doesn't need a stable
# ABI, because only `PlatformExecutors` imports it, with `internal import`, so
# build it without library evolution (`IS_FRAGILE`), like Foundation builds
# `_FoundationCollections`.
add_swift_target_library(swift_PlatformExecutorsCollections ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB IS_FRAGILE
  ${PLATFORM_EXECUTORS_COLLECTIONS_SOURCES}

  SWIFT_MODULE_DEPENDS_ANDROID Android
  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_LINUX_STATIC Musl
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_WINDOWS CRT

  LINK_LIBRARIES swiftCore

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    ${swift_platform_executors_collections_compile_flags}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

  # Only `PlatformExecutors` uses it, which isn't built for Apple platforms.
  TARGET_SDKS ANDROID LINUX LINUX_STATIC FREEBSD WINDOWS
  INSTALL_IN_COMPONENT stdlib
)
