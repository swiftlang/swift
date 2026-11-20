#===--- CMakeLists.txt - Platform executors library ------------------------===#
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

# The executors of the platforms other than the Apple platforms, such as the
# `pthread` and Win32 executors, and their `OperationScheduler`
# implementations. It re-exports `_PlatformExecutorsCore`.
#
# On the Apple platforms, the executors come from the Dispatch overlay instead.
# The sources are in the swift-platform-executors repository.
set(_platform_executors_source_dir "${SWIFT_PLATFORM_EXECUTORS_SOURCE_DIR}/Sources")
file(GLOB_RECURSE _PLATFORM_EXECUTORS_SOURCES CONFIGURE_DEPENDS
  "${_platform_executors_source_dir}/PlatformExecutors/*.swift"
  "${_platform_executors_source_dir}/CPlatformExecutors/*.c")
# The Dispatch executors are only for the Apple platforms, where the executors
# come from the Dispatch overlay instead. Their sources are guarded with
# `#if canImport(Darwin)`, but leave them out so that's explicit.
list(FILTER _PLATFORM_EXECUTORS_SOURCES EXCLUDE REGEX "/Sources/PlatformExecutors/Dispatch/")
set(PLATFORM_EXECUTORS_SOURCES)
foreach(source ${_PLATFORM_EXECUTORS_SOURCES})
  file(TO_CMAKE_PATH "${source}" source)
  list(APPEND PLATFORM_EXECUTORS_SOURCES ${source})
endforeach()
message(STATUS "Using swift-platform-executors for PlatformExecutors (${SWIFT_PLATFORM_EXECUTORS_SOURCE_DIR}).")

# The package's C shim `CPlatformExecutors` is built here as the module
# `_PlatformExecutorsCShims`, so that it doesn't clash with the package's own
# module when the package is built with this toolchain. The package's sources
# import and qualify names with `CPlatformExecutors`, so they're compiled with
# `-module-alias CPlatformExecutors=_PlatformExecutorsCShims`.
#
# `PlatformExecutors` imports the shim with `internal import`. Clients of the
# builds with library evolution don't load it, but clients of the builds
# without library evolution, such as the static Linux SDK (`LINUX_STATIC`), do.
# So, like Foundation's `_FoundationCShims`, its headers and a module map go to
# `lib/swift/_PlatformExecutorsCShims`, in the build directory and in the
# installation. The compiler searches its resource directory (`lib/swift`) for
# C modules, so neither this build nor clients need any flags to find it.
set(_platform_executors_c_include_dir "${_platform_executors_source_dir}/CPlatformExecutors/include")
file(GLOB _platform_executors_c_headers CONFIGURE_DEPENDS
  "${_platform_executors_c_include_dir}/*.h")
set(_platform_executors_c_module_map "${SWIFTLIB_DIR}/_PlatformExecutorsCShims/module.modulemap")
set(_platform_executors_c_module_dirs "${SWIFTLIB_DIR}/_PlatformExecutorsCShims")
if(SWIFT_BUILD_STATIC_STDLIB)
  list(APPEND _platform_executors_c_module_dirs "${SWIFTSTATICLIB_DIR}/_PlatformExecutorsCShims")
endif()
foreach(module_dir ${_platform_executors_c_module_dirs})
  foreach(header ${_platform_executors_c_headers})
    get_filename_component(header_name "${header}" NAME)
    configure_file("${header}" "${module_dir}/${header_name}" COPYONLY)
  endforeach()
  file(CONFIGURE OUTPUT "${module_dir}/module.modulemap" CONTENT [[
module _PlatformExecutorsCShims {
  header "CPlatformExecutors.h"
  export *
}
]])
endforeach()

swift_install_in_component(
  FILES ${_platform_executors_c_headers} "${_platform_executors_c_module_map}"
  DESTINATION "lib/swift/_PlatformExecutorsCShims"
  COMPONENT stdlib)
if(SWIFT_BUILD_STATIC_STDLIB)
  swift_install_in_component(
    FILES ${_platform_executors_c_headers} "${_platform_executors_c_module_map}"
    DESTINATION "lib/swift_static/_PlatformExecutorsCShims"
    COMPONENT stdlib)
endif()

# The Swift settings of the Package.swift of swift-platform-executors, with the
# `ExperimentalIO` trait enabled, the same as for `_PlatformExecutorsCore`.
# `BuiltinModule` is for `Builtin.stackAlloc`, and
# `AllowRuntimeSymbolDeclarations` for the `@_silgen_name` declarations of
# runtime functions.
set(swift_platform_executors_compile_flags
  -swift-version 6
  -package-name swift-platform-executors
  -enable-upcoming-feature ExistentialAny
  -enable-upcoming-feature MemberImportVisibility
  -enable-upcoming-feature InternalImportsByDefault
  -enable-upcoming-feature NonisolatedNonsendingByDefault
  -enable-experimental-feature Lifetimes
  -enable-experimental-feature BuiltinModule
  -enable-experimental-feature AllowRuntimeSymbolDeclarations
  -strict-memory-safety
  -D ExperimentalIO
  -Xfrontend -import-module -Xfrontend _Concurrency
  -module-alias CPlatformExecutors=_PlatformExecutorsCShims
  # The stdlib build passes `-resource-dir`, which finds the module map, but
  # name it explicitly so this doesn't depend on that.
  -Xcc -fmodule-map-file=${_platform_executors_c_module_map})

if(SWIFT_BUILD_STATIC_STDLIB)
  # Explicitly autolink the private collections module, because it is only
  # imported with `internal import`.
  list(APPEND swift_platform_executors_compile_flags
    -Xfrontend -public-autolink-library -Xfrontend swift_PlatformExecutorsCollections)
endif()

add_swift_target_library(swiftPlatformExecutors ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  ${PLATFORM_EXECUTORS_SOURCES}

  SWIFT_MODULE_DEPENDS _PlatformExecutorsCore _PlatformExecutorsCollections _Concurrency Synchronization
  SWIFT_MODULE_DEPENDS_ANDROID Android
  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_LINUX_STATIC Musl
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_WINDOWS CRT WinSDK

  LINK_LIBRARIES swiftCore swift_PlatformExecutorsCollections

  C_COMPILE_FLAGS
    -D_GNU_SOURCE
    -I${_platform_executors_c_include_dir}
  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    ${swift_platform_executors_compile_flags}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

  TARGET_SDKS ANDROID LINUX LINUX_STATIC FREEBSD WINDOWS
  INSTALL_IN_COMPONENT stdlib
)
