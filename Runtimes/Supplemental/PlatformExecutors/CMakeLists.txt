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
project(SwiftPlatformExecutors
  LANGUAGES Swift C
  VERSION ${SWIFT_RUNTIME_VERSION})

if(NOT PROJECT_IS_TOP_LEVEL)
  message(SEND_ERROR "Swift PlatformExecutors must build as a standalone project")
endif()

# On the Apple platforms, the executors come from the Dispatch overlay instead.
if(APPLE)
  message(FATAL_ERROR "Swift PlatformExecutors is only for the platforms other than the Apple platforms")
endif()

set(CMAKE_C_VISIBILITY_PRESET "hidden")
set(CMAKE_POSITION_INDEPENDENT_CODE YES)

set(${PROJECT_NAME}_SWIFTC_SOURCE_DIR
  "${PROJECT_SOURCE_DIR}/../../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

# `_PlatformExecutorsCollections` is built in place from the swift-collections
# checkout, like swift-foundation builds `_FoundationCollections`.
set(${PROJECT_NAME}_SWIFT_COLLECTIONS_SOURCE_DIR
  "${${PROJECT_NAME}_SWIFTC_SOURCE_DIR}/../swift-collections"
  CACHE PATH "Path to the swift-collections checkout")
if(NOT IS_DIRECTORY "${${PROJECT_NAME}_SWIFT_COLLECTIONS_SOURCE_DIR}/Sources/BasicContainers")
  message(FATAL_ERROR "Swift PlatformExecutors needs the swift-collections checkout, which is not at ${${PROJECT_NAME}_SWIFT_COLLECTIONS_SOURCE_DIR}")
endif()

# The experimental io_uring backend on Linux with glibc, the
# `ExperimentalIOUring` trait of swift-platform-executors. It uses swift-system,
# built from the swift-system checkout as private modules.
option(${PROJECT_NAME}_ENABLE_IO_URING
  "Enable the experimental io_uring backend on Linux with glibc" OFF)
set(${PROJECT_NAME}_SWIFT_SYSTEM_SOURCE_DIR
  "${${PROJECT_NAME}_SWIFTC_SOURCE_DIR}/../swift-system"
  CACHE PATH "Path to the swift-system checkout")
if(${PROJECT_NAME}_ENABLE_IO_URING)
  if(NOT CMAKE_SYSTEM_NAME STREQUAL "Linux")
    message(FATAL_ERROR "The io_uring backend of Swift PlatformExecutors is only for Linux")
  endif()
  if(NOT IS_DIRECTORY "${${PROJECT_NAME}_SWIFT_SYSTEM_SOURCE_DIR}/Sources/SystemSockets")
    message(FATAL_ERROR "The io_uring backend of Swift PlatformExecutors needs a swift-system checkout with SystemSockets, which is not at ${${PROJECT_NAME}_SWIFT_SYSTEM_SOURCE_DIR}")
  endif()
endif()

# Hook point for vendor-specific extensions to the build system
# Allowed extension points:
#   - DefaultSettings.cmake
#   - Settings.cmake
set(${PROJECT_NAME}_VENDOR_MODULE_DIR "${CMAKE_SOURCE_DIR}/../cmake/modules/vendor"
  CACHE FILEPATH "Location for private build system extension")

find_package(SwiftCore REQUIRED)
find_package(SwiftOverlay REQUIRED)
find_package(SwiftSynchronization REQUIRED)
find_package(SwiftPlatformExecutorsCore REQUIRED)

include(GNUInstallDirs)

include(AvailabilityMacros)
include(EmitSwiftInterface)
include(InstallSwiftInterface)
include(PlatformInfo)
include(ResourceEmbedding)
include(FramePointers)

option(${PROJECT_NAME}_INSTALL_NESTED_SUBDIR "Install libraries under a platform and architecture subdirectory" ON)
set(${PROJECT_NAME}_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}>" CACHE STRING "")
set(${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}>" CACHE STRING "")

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/Settings.cmake" OPTIONAL)

option(${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION "Generate ABI resilient runtime libraries"
  ${SwiftCore_ENABLE_LIBRARY_EVOLUTION})

option(${PROJECT_NAME}_ENABLE_PRESPECIALIZATION "Enable generic metadata prespecialization"
  ${SwiftCore_ENABLE_PRESPECIALIZATION})

# `-enable-library-evolution` is only passed to `PlatformExecutors`, because
# `_PlatformExecutorsCollections` is built without library evolution.
add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>
  $<$<COMPILE_LANGUAGE:Swift>:-nostdlibimport>
  $<$<COMPILE_LANGUAGE:Swift>:-strict-memory-safety>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enforce-exclusivity=unchecked>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -target-min-inlining-version -Xfrontend min>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-concurrency-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-string-processing-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:-warn-implicit-overrides>"
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

add_subdirectory(_PlatformExecutorsCollections)
if(${PROJECT_NAME}_ENABLE_IO_URING)
  add_subdirectory(_PlatformExecutorsSystem)
endif()
add_subdirectory(PlatformExecutors)
