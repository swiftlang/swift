cmake_minimum_required(VERSION 3.29)

if($ENV{BUILD_NUMBER})
  math(EXPR BUILD_NUMBER "$ENV{BUILD_NUMBER} % 65535")
  set(BUILD_NUMBER ".${BUILD_NUMBER}")
endif()

project(SwiftSynchronization
  LANGUAGES Swift
  VERSION 6.1.0${BUILD_NUMBER})

if(NOT PROJECT_IS_TOP_LEVEL)
  message(SEND_ERROR "Swift Synchronization must build as a standalone project")
endif()

set(CMAKE_POSITION_INDEPENDENT_CODE YES)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/modules")

set(CMAKE_Swift_LANGUAGE_VERSION 5)

include(GNUInstallDirs)

set(${PROJECT_NAME}_SWIFTC_SOURCE_DIR
  "${PROJECT_SOURCE_DIR}/../../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

set(${PROJECT_NAME}_VENDOR_MODULE_DIR "${CMAKE_SOURCE_DIR}/../cmake/modules/vendor"
  CACHE FILEPATH "Location for private build system extension")

find_package(SwiftCore QUIET REQUIRED)

include(gyb)
include(ResourceEmbedding)
include(AvailabilityMacros)
include(PlatformInfo)

include("${${PROJECT_NAME}_VENDOR_MODULE_DIR}/Settings.cmake" OPTIONAL)

option(${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION "Generate ABI resilient runtime libraries"
  ${SwiftCore_ENABLE_LIBRARY_EVOLUTION})

option(${PROJECT_NAME}_ENABLE_PRESPECIALIZATION "Enable generic metadata prespecialization"
  ${SwiftCore_ENABLE_PRESPECIALIZATION})

include(CatalystSupport)
include(EmitSwiftInterface)
include(InstallSwiftInterface)

add_compile_options(
  "$<$<COMPILE_LANGUAGE:Swift>:-nostdlibimport>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_LIBRARY_EVOLUTION}>,$<COMPILE_LANGUAGE:Swift>>:-enable-library-evolution>"
  "$<$<AND:$<BOOL:${${PROJECT_NAME}_ENABLE_PRESPECIALIZATION}>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xfrontend -prespecialize-generic-metadata>"
  $<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>
  $<$<COMPILE_LANGUAGE:Swift>:-enable-builtin-module>
  $<$<COMPILE_LANGUAGE:Swift>:-strict-memory-safety>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature RawLayout>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature StaticExclusiveOnly>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Extern>")

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
  Cell.swift
  Mutex/Mutex.swift
  $<$<PLATFORM_ID:Darwin>:Mutex/DarwinImpl.swift>
  $<$<PLATFORM_ID:Linux>:Mutex/LinuxImpl.swift>
  $<$<PLATFORM_ID:WASI>:Mutex/SpinLoopHint.swift>
  $<$<PLATFORM_ID:WINDOWS>:Mutex/WindowsImpl.swift>)

set_target_properties(swiftSynchronization PROPERTIES
  Swift_MODULE_NAME Synchronization)

target_link_libraries(swiftSynchronization
  PRIVATE
    swiftCore
    $<$<PLATFORM_ID:Darwin>:swiftDarwin>)

set(${PROJECT_NAME}_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${Supplemental_INSTALL_NESTED_SUBDIR}>:/${Supplemental_PLATFORM_SUBDIR}/${Supplemental_ARCH_SUBDIR}>" CACHE STRING "")
set(${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${Supplemental_INSTALL_NESTED_SUBDIR}>:/${Supplemental_PLATFORM_SUBDIR}>" CACHE STRING "")

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
