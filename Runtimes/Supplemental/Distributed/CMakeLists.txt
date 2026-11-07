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
project(SwiftDistributed
  LANGUAGES C CXX Swift
  VERSION ${SWIFT_RUNTIME_VERSION})

# The compatibility shim for CMake <3.31 the other runtime projects carry, so
# that the threading package below sees WASI as Core does.
if(CMAKE_VERSION VERSION_LESS 3.31)
  if(CMAKE_SYSTEM_NAME STREQUAL "Generic" AND CMAKE_SYSTEM_PROCESSOR MATCHES "wasm32")
    set(CMAKE_SYSTEM_NAME WASI)
    set(WASI 1)
  endif()
endif()

if(NOT PROJECT_IS_TOP_LEVEL)
  message(SEND_ERROR "Swift Distributed must build as a standalone project")
endif()

set(CMAKE_POSITION_INDEPENDENT_CODE YES)

set(CMAKE_C_VISIBILITY_PRESET "hidden")
set(CMAKE_CXX_VISIBILITY_PRESET "hidden")
set(CMAKE_VISIBILITY_INLINES_HIDDEN YES)

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
include(CatalystSupport)
include(SwiftCallingConventions)

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
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SuppressedAssociatedTypesWithDefaults>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NonescapableTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Lifetimes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-upcoming-feature MemberImportVisibility>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enforce-exclusivity=unchecked>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -target-min-inlining-version -Xfrontend min>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-lexical-lifetimes=false>"
  "$<$<COMPILE_LANGUAGE:Swift>:-warn-implicit-overrides>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION}>,$<COMPILE_LANGUAGE:Swift>>:-enable-library-evolution>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_PRESPECIALIZATION}>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xfrontend -prespecialize-generic-metadata>")

find_package(SwiftSwiftDirectRuntime)

# LNK4049: symbol 'symbol' defined in 'filename.obj' is imported
# LNK4286: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj'
# LNK4217: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj' in function 'function'
#
# We cannot selectively filter the linker warnings as we do not use the MSVC
# frontned and `clang-cl` (and `clang`) currently do not support `/WX:nnnn`. As
# a compromise, treat all linker warnings as errors.
add_link_options($<$<PLATFORM_ID:Windows>:LINKER:/WX>)
# Ensure all symbols are fully resolved on Linux
add_link_options($<$<PLATFORM_ID:Android,Linux>:LINKER:-z,defs>)

# The threading package, so that the C++ sources and the threading objects
# below agree with the Core the library links: the package given on the
# command line, else the one Core's config file exports, else the one Core
# itself defaults to on the platform (Runtimes/Core/cmake/modules/
# DefaultSettings.cmake), which matches a Core built with its defaults. A Core
# found without its config file (FindSwiftCore.cmake, an SDK's or a
# toolchain's) exports nothing, so one built with another package needs its
# config file, or the package given. On Darwin the primitives are inline
# system calls, so when neither names a package it stays unset there and
# include/swift/Threading/Impl.h selects Darwin itself.
set(${PROJECT_NAME}_THREADING_PACKAGES DARWIN LINUX PTHREADS C11 WIN32 NONE)
if("${${PROJECT_NAME}_THREADING_PACKAGE}" STREQUAL "")
  set(${PROJECT_NAME}_THREADING_PACKAGE "${SwiftCore_THREADING_PACKAGE}")
endif()
if("${${PROJECT_NAME}_THREADING_PACKAGE}" STREQUAL "" AND NOT APPLE)
  if(LINUX)
    set(${PROJECT_NAME}_THREADING_PACKAGE LINUX)
  elseif(WASI)
    set(${PROJECT_NAME}_THREADING_PACKAGE NONE)
  elseif(ANDROID OR BSD OR CMAKE_SYSTEM_NAME STREQUAL "Emscripten")
    set(${PROJECT_NAME}_THREADING_PACKAGE PTHREADS)
  elseif(WIN32)
    set(${PROJECT_NAME}_THREADING_PACKAGE WIN32)
  endif()
  if(NOT "${${PROJECT_NAME}_THREADING_PACKAGE}" STREQUAL "")
    message(STATUS "${PROJECT_NAME}_THREADING_PACKAGE defaulted to "
      "${${PROJECT_NAME}_THREADING_PACKAGE}, the package Core defaults to on "
      "this platform, since the Core found exports none")
  endif()
endif()
if(NOT "${${PROJECT_NAME}_THREADING_PACKAGE}" STREQUAL "")
  set(${PROJECT_NAME}_THREADING_PACKAGE "${${PROJECT_NAME}_THREADING_PACKAGE}"
    CACHE STRING "Threading Package: DARWIN LINUX PTHREADS C11 WIN32 NONE")
  # The macros are spelled in uppercase; the cache keeps the spelling given.
  string(TOUPPER "${${PROJECT_NAME}_THREADING_PACKAGE}" ${PROJECT_NAME}_THREADING_PACKAGE)
  if(NOT ${PROJECT_NAME}_THREADING_PACKAGE IN_LIST ${PROJECT_NAME}_THREADING_PACKAGES)
    message(FATAL_ERROR "${PROJECT_NAME}_THREADING_PACKAGE is "
      "'${${PROJECT_NAME}_THREADING_PACKAGE}', expected DARWIN, LINUX, PTHREADS, "
      "C11, WIN32 or NONE")
  endif()
endif()

# DistributedActor.cpp uses the runtime's threading primitives, whose
# out-of-line parts a shared libswiftCore does not export, so the library
# carries the threading objects itself, as swift_Concurrency does, unless it
# links a static Core, whose archive supplies them. With no package (Darwin,
# above) the objects would be empty and are left out.
get_target_property(SwiftCore_LIBRARY_TYPE swiftCore TYPE)
if(NOT "${${PROJECT_NAME}_THREADING_PACKAGE}" STREQUAL ""
    AND NOT SwiftCore_LIBRARY_TYPE STREQUAL "STATIC_LIBRARY")
  add_library(swiftDistributedThreading OBJECT
    "${${PROJECT_NAME}_SWIFTC_SOURCE_DIR}/lib/Threading/C11.cpp"
    "${${PROJECT_NAME}_SWIFTC_SOURCE_DIR}/lib/Threading/Linux.cpp"
    "${${PROJECT_NAME}_SWIFTC_SOURCE_DIR}/lib/Threading/Pthreads.cpp"
    "${${PROJECT_NAME}_SWIFTC_SOURCE_DIR}/lib/Threading/WindowsThreading.cpp")
  target_compile_definitions(swiftDistributedThreading PRIVATE
    -DSWIFT_THREADING_${${PROJECT_NAME}_THREADING_PACKAGE})
  target_include_directories(swiftDistributedThreading PRIVATE
    "${${PROJECT_NAME}_SWIFTC_SOURCE_DIR}/include"
    "${${PROJECT_NAME}_PATH_TO_SWIFT_RUNTIME_HEADERS}"
    # For llvm/support headers
    "${PROJECT_SOURCE_DIR}/include")
  # swiftCore for `swift/Runtime/CMakeConfig.h`, as swiftDistributed gets it.
  target_link_libraries(swiftDistributedThreading PRIVATE
    swiftShims
    swiftCore)
endif()

add_library(swiftDistributed
  DistributedActor.cpp
  ThreadingError.cpp
  DistributedActor.swift
  DistributedActorSystem.swift
  DistributedAssertions.swift
  DistributedDefaultExecutor.swift
  DistributedMacros.swift
  DistributedMetadata.swift
  LocalTestingDistributedActorSystem.swift)

set_target_properties(swiftDistributed PROPERTIES
  Swift_MODULE_NAME Distributed)
cmake_policy(GET CMP0157 Policy157Enabled)
if(Policy157Enabled STREQUAL NEW)
  set_target_properties(swiftDistributed PROPERTIES
    LINKER_LANGUAGE CXX)
  # `CMAKE_STATIC_LIBRARY_PREFIX_Swift` will not impact the prefix for this
  # library because we have changed the linker language to C++. This is required
  # for Windows to properly reference this library.
  if(NOT BUILD_SHARED_LIBS)
    set_target_properties(swiftDistributed PROPERTIES
      PREFIX lib)
  endif()
endif()

target_compile_definitions(swiftDistributed PRIVATE
  $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_TARGET_LIBRARY_NAME=swiftDistributed>
  $<$<AND:$<COMPILE_LANGUAGE:C,CXX>,$<BOOL:${${PROJECT_NAME}_THREADING_PACKAGE}>>:-DSWIFT_THREADING_${${PROJECT_NAME}_THREADING_PACKAGE}>)
target_compile_options(swiftDistributed PRIVATE
  $<$<COMPILE_LANGUAGE:Swift>:-parse-stdlib>
  $<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature AllowUnsafeAttribute>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-sil-opaque-values>")

if(APPLE AND BUILD_SHARED_LIBS)
  target_link_options(swiftDistributed PRIVATE
    "SHELL:-Xlinker -headerpad_max_install_names")
endif()

target_include_directories(swiftDistributed PRIVATE
  # FIXME: Use of `swift/Runtime/...`, `swift/ABI/...`, and `swift/Demangling/...`
  "${${PROJECT_NAME}_SWIFTC_SOURCE_DIR}/include"
  # For `swift/runtime/CMakeConfig.h`, which should be generated
  # by the Core build system.
  "${${PROJECT_NAME}_PATH_TO_SWIFT_RUNTIME_HEADERS}"
  # For llvm/support headers
  "${PROJECT_SOURCE_DIR}/include")

target_link_libraries(swiftDistributed PRIVATE
  swiftShims
  swiftCore
  swift_Concurrency
  swift_Builtin_float
  $<$<TARGET_EXISTS:swiftDistributedThreading>:swiftDistributedThreading>
  $<$<PLATFORM_ID:Android>:swiftAndroid>
  $<$<PLATFORM_ID:Windows>:swiftWinSDK>
  $<$<BOOL:${SwiftSwiftDirectRuntime_FOUND}>:swiftSwiftDirectRuntime>)

install(TARGETS swiftDistributed
  EXPORT SwiftDistributedTargets
  COMPONENT ${PROJECT_NAME}_runtime
  ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftDistributed)
install_swift_interface(swiftDistributed)

# Configure plist creation for Darwin platforms.
generate_plist(swiftDistributed "${CMAKE_PROJECT_VERSION}" swiftDistributed)
embed_manifest(swiftDistributed)

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/swiftDistributed.cmake" OPTIONAL)
