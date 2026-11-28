cmake_minimum_required(VERSION 3.29)
# TODO before requiring CMake 4.1 or later
# and/or enforcing CMP0195, please check/update
# the implementation  of `emit_swift_interface`
# in `EmitSwiftInterface.cmake`
# to ensure it keeps laying down nested swiftmodule folders

list(APPEND CMAKE_MODULE_PATH
  "${CMAKE_SOURCE_DIR}/../cmake/modules"
  "${CMAKE_SOURCE_DIR}/../../cmake/modules")

include(LanguageVersions)
include(SwiftProjectVersion)
project(SwiftPlatformExecutorsCore
  LANGUAGES Swift
  VERSION ${SWIFT_RUNTIME_VERSION})

# FIXME(compnerd) this is a workaround for `GNUInstallDirs` which cannot be used
# with a pure Swift project.
enable_language(C)

if(NOT PROJECT_IS_TOP_LEVEL)
  message(SEND_ERROR "Swift PlatformExecutorsCore must build as a standalone project")
endif()

set(${PROJECT_NAME}_SWIFTC_SOURCE_DIR
  "${PROJECT_SOURCE_DIR}/../../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

# Hook point for vendor-specific extensions to the build system
# Allowed extension points:
#   - DefaultSettings.cmake
#   - Settings.cmake
set(${PROJECT_NAME}_VENDOR_MODULE_DIR "${CMAKE_SOURCE_DIR}/../cmake/modules/vendor"
  CACHE FILEPATH "Location for private build system extension")

find_package(SwiftCore REQUIRED)
find_package(SwiftOverlay REQUIRED)

include(GNUInstallDirs)

include(AvailabilityMacros)
include(EmitSwiftInterface)
include(InstallSwiftInterface)
include(PlatformInfo)
include(ResourceEmbedding)
include(FramePointers)
include(CatalystSupport)

option(${PROJECT_NAME}_INSTALL_NESTED_SUBDIR "Install libraries under a platform and architecture subdirectory" ON)
set(${PROJECT_NAME}_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}>" CACHE STRING "")
set(${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}>" CACHE STRING "")

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/Settings.cmake" OPTIONAL)

option(${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION "Generate ABI resilient runtime libraries"
  ${SwiftCore_ENABLE_LIBRARY_EVOLUTION})

option(${PROJECT_NAME}_ENABLE_PRESPECIALIZATION "Enable generic metadata prespecialization"
  ${SwiftCore_ENABLE_PRESPECIALIZATION})

add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>
  $<$<COMPILE_LANGUAGE:Swift>:-nostdlibimport>
  $<$<COMPILE_LANGUAGE:Swift>:-strict-memory-safety>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Werror StrictMemorySafety>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enforce-exclusivity=unchecked>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -target-min-inlining-version -Xfrontend min>"
  "$<$<COMPILE_LANGUAGE:Swift>:-warn-implicit-overrides>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION}>,$<COMPILE_LANGUAGE:Swift>>:-enable-library-evolution>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_PRESPECIALIZATION}>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xfrontend -prespecialize-generic-metadata>")

find_package(SwiftSwiftDirectRuntime)

# LNK4049: symbol 'symbol' defined in 'filename.obj' is imported
# LNK4286: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj'
# LNK4217: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj' in function 'function'
#
# We cannot selectively filter the linker warnings as we do not use the MSVC
# frontend and `clang-cl` (and `clang`) currently do not support `/WX:nnnn`. As
# a compromise, treat all linker warnings as errors.
add_link_options($<$<PLATFORM_ID:Windows>:LINKER:/WX>)
# Ensure all symbols are fully resolved on Linux
add_link_options($<$<PLATFORM_ID:Android,Linux>:LINKER:-z,defs>)

# The protocols and vocabulary types of the I/O interfaces of the platform
# executors, such as `FileOperationScheduler`. The Dispatch overlay conforms to
# them, so this module must not depend on anything above `_Concurrency`.
# Resync.cmake copies the sources from the `_PlatformExecutorsCore` target of
# the swift-platform-executors checkout.
add_library(swift_PlatformExecutorsCore
  IO/LowLevel/FileOperationScheduler.swift
  IO/LowLevel/IOError.swift
  IO/LowLevel/IPv4Address.swift
  IO/LowLevel/IPv6Address.swift
  IO/LowLevel/LeasedBuffer.swift
  IO/LowLevel/OperationScheduler.swift
  IO/LowLevel/ReceivedDatagram.swift
  IO/LowLevel/SocketAddress.swift
  IO/LowLevel/SocketShutdownDirection.swift
  IO/LowLevel/TCPSocketOperationScheduler.swift
  IO/LowLevel/UDPSocketOperationScheduler.swift)
set_target_properties(swift_PlatformExecutorsCore PROPERTIES
  Swift_MODULE_NAME _PlatformExecutorsCore
  Swift_LANGUAGE_VERSION 6)
# The Swift settings of the Package.swift of swift-platform-executors, with the
# `ExperimentalIO` trait enabled. `NonisolatedNonsendingByDefault` changes the
# ABI of the `async` requirements, so it must match the package build.
# The sources rely on the implicit import of `_Concurrency` for `async`. It is
# imported explicitly, and nothing else implicitly, so that the interface only
# imports `Swift` and `_Concurrency`, like the stdlib/public build.
target_compile_options(swift_PlatformExecutorsCore PRIVATE
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-upcoming-feature ExistentialAny>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-upcoming-feature MemberImportVisibility>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-upcoming-feature InternalImportsByDefault>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-upcoming-feature NonisolatedNonsendingByDefault>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Lifetimes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-D ExperimentalIO>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-concurrency-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-string-processing-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -import-module -Xfrontend _Concurrency>")
# The declarations use the `SwiftPlatformExecutors` availability macros, keyed
# by the version of swift-platform-executors. Define each of them with the
# availability of the `SwiftStdlib` macro that
# utils/platform-executors-availability-macros.def maps it to.
set(_platform_executors_availability_file
  "${${PROJECT_NAME}_SWIFTC_SOURCE_DIR}/utils/platform-executors-availability-macros.def")
set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
  "${_platform_executors_availability_file}")
file(STRINGS "${_platform_executors_availability_file}"
  _platform_executors_availability REGEX "^SwiftPlatformExecutors ")
file(STRINGS "${${PROJECT_NAME}_SWIFTC_SOURCE_DIR}/utils/availability-macros.def"
  _platform_executors_stdlib_availability REGEX "^SwiftStdlib ")
foreach(mapping ${_platform_executors_availability})
  # For example `SwiftPlatformExecutors 0.1.0:SwiftStdlib 6.5`.
  string(REGEX MATCH "^([^:]+):[ \t]*(.*[^ \t])[ \t]*$" _ "${mapping}")
  set(macro "${CMAKE_MATCH_1}")
  set(stdlib_macro "${CMAKE_MATCH_2}:")
  string(LENGTH "${stdlib_macro}" stdlib_macro_length)
  set(definition)
  foreach(stdlib_definition ${_platform_executors_stdlib_availability})
    string(FIND "${stdlib_definition}" "${stdlib_macro}" position)
    if(position EQUAL 0)
      string(SUBSTRING "${stdlib_definition}" ${stdlib_macro_length} -1 availability)
      set(definition "${macro}:${availability}")
    endif()
  endforeach()
  if(NOT definition)
    message(FATAL_ERROR "'${mapping}' in ${_platform_executors_availability_file} refers to a macro that is not in utils/availability-macros.def")
  endif()
  target_compile_options(swift_PlatformExecutorsCore PRIVATE
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -define-availability -Xfrontend \"${definition}\">")
endforeach()
# For the libraries that import the module from another project, such as
# `PlatformExecutors`.
target_include_directories(swift_PlatformExecutorsCore INTERFACE
  "$<$<COMPILE_LANGUAGE:Swift>:$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${SwiftCore_INSTALL_SWIFTMODULEDIR}>>")
target_link_libraries(swift_PlatformExecutorsCore PRIVATE
  swiftCore
  swift_Concurrency
  $<$<BOOL:${SwiftSwiftDirectRuntime_FOUND}>:swiftSwiftDirectRuntime>)

install(TARGETS swift_PlatformExecutorsCore
  EXPORT SwiftPlatformExecutorsCoreTargets
  COMPONENT ${PROJECT_NAME}_runtime
  ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swift_PlatformExecutorsCore)
install_swift_interface(swift_PlatformExecutorsCore)

# Inter-project install info, for the libraries that conform to the protocols,
# such as the Dispatch overlay.
export(EXPORT SwiftPlatformExecutorsCoreTargets
  FILE "cmake/SwiftPlatformExecutorsCore/SwiftPlatformExecutorsCoreTargets.cmake")
install(EXPORT SwiftPlatformExecutorsCoreTargets
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftPlatformExecutorsCore"
  FILE "SwiftPlatformExecutorsCoreTargets.cmake"
  COMPONENT SwiftPlatformExecutorsCoreCMake)
include(CMakePackageConfigHelpers)
configure_package_config_file("${CMAKE_CURRENT_SOURCE_DIR}/cmake/interface/SwiftPlatformExecutorsCoreConfig.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftPlatformExecutorsCore/SwiftPlatformExecutorsCoreConfig.cmake"
  INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftPlatformExecutorsCore")
write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftPlatformExecutorsCore/SwiftPlatformExecutorsCoreConfigVersion.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY ExactVersion)
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftPlatformExecutorsCore/SwiftPlatformExecutorsCoreConfig.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftPlatformExecutorsCore/SwiftPlatformExecutorsCoreConfigVersion.cmake"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftPlatformExecutorsCore")

# Configure plist creation for Darwin platforms.
generate_plist(swift_PlatformExecutorsCore "${CMAKE_PROJECT_VERSION}" swift_PlatformExecutorsCore)
embed_manifest(swift_PlatformExecutorsCore)

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/swift_PlatformExecutorsCore.cmake" OPTIONAL)
