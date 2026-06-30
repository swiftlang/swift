cmake_minimum_required(VERSION 3.29)
# TODO before requiring CMake 4.1 or later
# and/or enforcing CMP0195, please check/update
# the implementation  of `emit_swift_interface`
# in `EmitSwiftInterface.cmake`
# to ensure it keeps laying down nested swiftmodule folders

list(APPEND CMAKE_MODULE_PATH
  "${CMAKE_SOURCE_DIR}/../cmake/modules"
  "${CMAKE_SOURCE_DIR}/../../cmake/modules")

include(SwiftProjectVersion)
project(SwiftCOM
  LANGUAGES Swift
  VERSION ${SWIFT_RUNTIME_VERSION})

# FIXME(compnerd) add a compatibility shim for WASI on older CMake releases.
if(CMAKE_VERSION VERSION_LESS 3.31)
  if(CMAKE_SYSTEM_NAME STREQUAL "Generic" AND CMAKE_SYSTEM_PROCESSOR MATCHES "wasm32")
    set(CMAKE_SYSTEM_NAME WASI)
    set(WASI 1)
  endif()
endif()

if(NOT PROJECT_IS_TOP_LEVEL)
  message(SEND_ERROR "Swift Differentiation must build as a standalone project")
endif()

set(CMAKE_POSITION_INDEPENDENT_CODE YES)
set(CMAKE_Swift_LANGUAGE_VERSION 5)

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
find_package(SwiftOverlay)

include(GNUInstallDirs)

include(AvailabilityMacros)
include(PlatformInfo)
include(ResourceEmbedding)

option(${PROJECT_NAME}_INSTALL_NESTED_SUBDIR "Install libraries under a platform and architecture subdirectory" ON)
set(${PROJECT_NAME}_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}>" CACHE STRING "")
set(${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}>" CACHE STRING "")

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/Settings.cmake" OPTIONAL)

option(${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION "Generate ABI resilient runtime libraries"
  ${SwiftCore_ENABLE_LIBRARY_EVOLUTION})

add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:SHELL:-explicit-module-build>
  $<$<COMPILE_LANGUAGE:Swift>:-nostdlibimport>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature MemberImportVisibility>"
  $<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION}>,$<COMPILE_LANGUAGE:Swift>>:-enable-library-evolution>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-concurrency-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-string-processing-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-experimental-com-interop>")

find_package(SwiftSwiftDirectRuntime)

# LNK4049: symbol 'symbol' defined in 'filename.obj' is imported
# LNK4286: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj'
# LNK4217: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj' in function 'function'
#
# We cannot selectively filter the linker warnings as we do not use the MSVC
# frontned and `clang-cl` (and `clang`) currently do not support `/WX:nnnn`. As
# a compromise, treat all linker warnings as errors.
# FIXME(#83444) - enable this once we fix DLL storage for
# `_fatalErrorForwardModeDifferentiationDisabled`
# add_link_options($<$<PLATFORM_ID:Windows>:LINKER:/WX>)
# Ensure all symbols are fully resolved on Linux
add_link_options($<$<PLATFORM_ID:Android,Linux>:LINKER:-z,defs>)

add_subdirectory(Sources)

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/swift_COM.cmake" OPTIONAL)
