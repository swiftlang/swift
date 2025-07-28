cmake_minimum_required(VERSION 3.29)
# TODO before requiring CMake 4.1 or later
# and/or enforcing CMP0195, please check/update
# the implementation  of `emit_swift_interface`
# in `EmitSwiftInterface.cmake`
# to ensure it keeps laying down nested swiftmodule folders

if(POLICY CMP0157 AND CMAKE_Swift_COMPILER_USE_OLD_DRIVER)
  cmake_policy(SET CMP0157 OLD)
endif()

if($ENV{BUILD_NUMBER})
  math(EXPR BUILD_NUMBER "$ENV{BUILD_NUMBER} % 65535")
  set(BUILD_NUMBER ".${BUILD_NUMBER}")
endif()
project(SwiftSynchronization
  LANGUAGES Swift
  VERSION 6.1.0${BUILD_NUMBER})
# FIXME(compnerd) this is a workaround for `GNUInstallDirs` which cannot be used
# with a pure Swift project.
enable_language(C)

if(NOT PROJECT_IS_TOP_LEVEL)
  message(SEND_ERROR "Swift Synchronization must build as a standalone project")
endif()

set(CMAKE_POSITION_INDEPENDENT_CODE YES)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/modules")

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
include(EmitSwiftInterface)
include(InstallSwiftInterface)
include(PlatformInfo)
include(gyb)
include(ResourceEmbedding)
include(CatalystSupport)

option(${PROJECT_NAME}_INSTALL_NESTED_SUBDIR "Install libraries under a platform and architecture subdirectory" ON)
set(${PROJECT_NAME}_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}>" CACHE STRING "")
set(${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${${PROJECT_NAME}_INSTALL_NESTED_SUBDIR}>:/${${PROJECT_NAME}_PLATFORM_SUBDIR}>" CACHE STRING "")

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/Settings.cmake" OPTIONAL)

option(${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION "Generate ABI resilient runtime libraries"
  ${SwiftCore_ENABLE_LIBRARY_EVOLUTION})

option(${PROJECT_NAME}_ENABLE_PRESPECIALIZATION "Enable generic metadata prespecialization"
  ${SwiftCore_ENABLE_PRESPECIALIZATION})

option(${PROJECT_NAME}_SINGLE_THREADED_MODE "Build Synchronization assuming it will be used in an environment with only a single thread" OFF)

add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>
  $<$<COMPILE_LANGUAGE:Swift>:-nostdlibimport>
  $<$<COMPILE_LANGUAGE:Swift>:-enable-builtin-module>
  $<$<COMPILE_LANGUAGE:Swift>:-strict-memory-safety>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NoncopyableGenerics2>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SuppressedAssociatedTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SE427NoInferenceOnExtension>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NonescapableTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature LifetimeDependence>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature InoutLifetimeDependence>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature LifetimeDependenceMutableAccessors>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-upcoming-feature MemberImportVisibility>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature RawLayout>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature StaticExclusiveOnly>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Extern>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-runtime-compatibility-version none>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-autolinking-runtime-compatibility-dynamic-replacements>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-autolinking-runtime-compatibility-concurrency>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-concurrency-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-string-processing-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enforce-exclusivity=unchecked>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -target-min-inlining-version -Xfrontend min>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-lexical-lifetimes=false>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION}>,$<COMPILE_LANGUAGE:Swift>>:-enable-library-evolution>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_PRESPECIALIZATION}>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xfrontend -prespecialize-generic-metadata>")

gyb_expand(Atomics/AtomicIntegers.swift.gyb Atomics/AtomicIntegers.swift)
gyb_expand(Atomics/AtomicStorage.swift.gyb Atomics/AtomicStorage.swift)

add_library(swiftSynchronization
  Atomics/Atomic.swift
  Atomics/AtomicBool.swift
  Atomics/AtomicFloats.swift
  Atomics/AtomicLazyReference.swift
  Atomics/AtomicMemoryOrderings.swift
  Atomics/AtomicOptional.swift
  Atomics/AtomicPointers.swift
  Atomics/AtomicRepresentable.swift
  Atomics/WordPair.swift
  Atomics/AtomicStorage.swift
  Atomics/AtomicIntegers.swift
  Cell.swift)

# Determine Mutex definition
if(${PROJECT_NAME}_SINGLE_THREADED_MODE)
  target_sources(swiftSynchronization PRIVATE
    Mutex/MutexUnavailable.swift)
else()
  target_sources(swiftSynchronization PRIVATE
    Mutex/Mutex.swift
    $<$<PLATFORM_ID:Darwin>:Mutex/DarwinImpl.swift>
    $<$<PLATFORM_ID:Android,Linux>:Mutex/LinuxImpl.swift>
    $<$<PLATFORM_ID:Android,WASI>:Mutex/SpinLoopHint.swift>
    $<$<PLATFORM_ID:Windows>:Mutex/WindowsImpl.swift>)
endif()

set_target_properties(swiftSynchronization PROPERTIES
  Swift_MODULE_NAME Synchronization)

target_link_libraries(swiftSynchronization PRIVATE
  swiftCore
  $<$<PLATFORM_ID:Android>:SwiftAndroid>
  $<$<PLATFORM_ID:Darwin>:swiftDarwin>
  $<$<PLATFORM_ID:Windows>:ClangModules>)

install(TARGETS swiftSynchronization
  EXPORT SwiftSynchronizationTargets
  COMPONENT ${PROJECT_NAME}_runtime
  ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swiftSynchronization)
install_swift_interface(swiftSynchronization)

# Configure plist creation for Darwin platforms.
generate_plist("${CMAKE_PROJECT_NAME}" "${CMAKE_PROJECT_VERSION}" swiftSynchronization)
embed_manifest(swiftSynchronization)

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/swiftSynchronization.cmake" OPTIONAL)
