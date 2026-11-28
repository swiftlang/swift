#===--- CMakeLists.txt - Platform executors swift-system libraries ---------===#
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

# The parts of swift-system that the experimental io_uring backend of
# `PlatformExecutors` uses, built from the swift-system checkout as private
# modules, like `_PlatformExecutorsCollections`:
#
# - `_PlatformExecutorsSystem`, from the `SystemPackage` target.
# - `_PlatformExecutorsSystemSockets`, from the `SystemSockets` target.
# - `_PlatformExecutorsSystemCShims`, the C module `CSystem`.
#
# The sources keep importing `SystemPackage`, `SystemSockets` and `CSystem`,
# and are compiled with `-module-alias` to the private names, so that the
# toolchain doesn't ship modules that clash with the swift-system package. Only
# `PlatformExecutors` may import them, with `internal import`, so they don't
# appear in its interface. swift-system isn't built with library evolution, so
# they are built without it (`IS_FRAGILE`).
#
# io_uring is only for Linux with glibc.
set(_system_source_dir "${SWIFT_PATH_TO_SWIFT_SYSTEM_SOURCE}/Sources")
message(STATUS "Using swift-system for _PlatformExecutorsSystem (${SWIFT_PATH_TO_SWIFT_SYSTEM_SOURCE}).")

# The C module, with the headers and the module map of `CSystem` under its
# private name, in `lib/swift/_PlatformExecutorsSystemCShims` of the build
# directory and of the installation, like `_PlatformExecutorsCShims`.
set(_system_c_include_dir "${_system_source_dir}/CSystem/include")
file(GLOB _system_c_headers CONFIGURE_DEPENDS "${_system_c_include_dir}/*.h")
set(_system_c_module_dir "${SWIFTLIB_DIR}/_PlatformExecutorsSystemCShims")
set(_system_c_module_map "${_system_c_module_dir}/module.modulemap")
foreach(header ${_system_c_headers})
  get_filename_component(header_name "${header}" NAME)
  configure_file("${header}" "${_system_c_module_dir}/${header_name}" COPYONLY)
endforeach()
set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
  "${_system_c_include_dir}/module.modulemap")
file(READ "${_system_c_include_dir}/module.modulemap" _system_c_module_map_contents)
string(REGEX REPLACE "module[ \t]+CSystem[ \t]*{" "module _PlatformExecutorsSystemCShims {"
  _system_c_module_map_contents "${_system_c_module_map_contents}")
file(CONFIGURE OUTPUT "${_system_c_module_map}" CONTENT "${_system_c_module_map_contents}")
swift_install_in_component(
  FILES ${_system_c_headers} "${_system_c_module_map}"
  DESTINATION "lib/swift/_PlatformExecutorsSystemCShims"
  COMPONENT stdlib)
if(SWIFT_BUILD_STATIC_STDLIB)
  set(_system_c_static_module_dir "${SWIFTSTATICLIB_DIR}/_PlatformExecutorsSystemCShims")
  foreach(header ${_system_c_headers})
    get_filename_component(header_name "${header}" NAME)
    configure_file("${header}" "${_system_c_static_module_dir}/${header_name}" COPYONLY)
  endforeach()
  file(CONFIGURE OUTPUT "${_system_c_static_module_dir}/module.modulemap"
    CONTENT "${_system_c_module_map_contents}")
  swift_install_in_component(
    FILES ${_system_c_headers} "${_system_c_module_map}"
    DESTINATION "lib/swift_static/_PlatformExecutorsSystemCShims"
    COMPONENT stdlib)
endif()

# The `System X.Y.Z` availability macros of swift-system. They only matter on
# the Apple platforms, so define each version that the Package.swift of
# swift-system lists with the availability of the sources there. They are added
# to the availability definitions of this directory, so add_swift_target_library
# passes them like the `SwiftStdlib` macros.
set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
  "${SWIFT_PATH_TO_SWIFT_SYSTEM_SOURCE}/Package.swift")
file(STRINGS "${SWIFT_PATH_TO_SWIFT_SYSTEM_SOURCE}/Package.swift"
  _system_availability REGEX "Available\\(\"[0-9.]+\"")
foreach(line ${_system_availability})
  string(REGEX MATCH "Available\\(\"([0-9.]+)\"" _ "${line}")
  list(APPEND SWIFT_STDLIB_AVAILABILITY_DEFINITIONS
    "System ${CMAKE_MATCH_1}:macOS 10.10, iOS 8.0, watchOS 2.0, tvOS 9.0, visionOS 1.0")
endforeach()

# The Swift settings of the Package.swift of swift-system.
set(_system_compile_flags
  -swift-version 6
  -package-name swift-system
  -D SYSTEM_PACKAGE
  -enable-experimental-feature Lifetimes
  -enable-upcoming-feature MemberImportVisibility
  -module-alias CSystem=_PlatformExecutorsSystemCShims
  -Xcc -fmodule-map-file=${_system_c_module_map})

file(GLOB_RECURSE _system_sources CONFIGURE_DEPENDS
  "${_system_source_dir}/System/*.swift")
add_swift_target_library(swift_PlatformExecutorsSystem ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB IS_FRAGILE
  ${_system_sources}
  "${_system_source_dir}/CSystem/shims.c"

  # `IORing` uses `Synchronization`.
  SWIFT_MODULE_DEPENDS Synchronization
  SWIFT_MODULE_DEPENDS_LINUX Glibc

  LINK_LIBRARIES swiftCore

  C_COMPILE_FLAGS
    -I${_system_c_include_dir}
  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    ${_system_compile_flags}
    -Xfrontend -allowable-client -Xfrontend PlatformExecutors
    -Xfrontend -allowable-client -Xfrontend _PlatformExecutorsSystemSockets
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

  TARGET_SDKS LINUX
  INSTALL_IN_COMPONENT stdlib
)

file(GLOB_RECURSE _system_sockets_sources CONFIGURE_DEPENDS
  "${_system_source_dir}/SystemSockets/*.swift")
add_swift_target_library(swift_PlatformExecutorsSystemSockets ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB IS_FRAGILE
  ${_system_sockets_sources}

  SWIFT_MODULE_DEPENDS _PlatformExecutorsSystem
  SWIFT_MODULE_DEPENDS_LINUX Glibc

  LINK_LIBRARIES swiftCore

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    ${_system_compile_flags}
    -module-alias SystemPackage=_PlatformExecutorsSystem
    -Xfrontend -allowable-client -Xfrontend PlatformExecutors
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

  TARGET_SDKS LINUX
  INSTALL_IN_COMPONENT stdlib
)
