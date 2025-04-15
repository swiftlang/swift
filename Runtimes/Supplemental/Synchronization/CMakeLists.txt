cmake_minimum_required(VERSION 3.29)

set(CMAKE_POSITION_INDEPENDENT_CODE YES) # ToDo: What does this do?

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/modules")

if($ENV{BUILD_NUMBER})
  math(EXPR BUILD_NUMBER "$ENV{BUILD_NUMBER} % 65535")
  set(BUILD_NUMBER ".${BUILD_NUMBER}")
endif()
project(SwiftSynchronization
  LANGUAGES Swift
  VERSION 6.1.0${BUILD_NUMBER})

include(GNUInstallDirs)

if(NOT PROJECT_IS_TOP_LEVEL)
  message(SEND_ERROR "Swift Synchronization must build as a standalone project")
endif()

set(${PROJECT_NAME}_SWIFTC_SOURCE_DIR
  "${PROJECT_SOURCE_DIR}/../../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

find_package(SwiftCore)
include(gyb)
include(AvailabilityMacros)

add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>
  $<$<COMPILE_LANGUAGE:Swift>:-enable-builtin-module> # Are these contradictory?
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
  Swift_MODULE_NAME Synchronization
  Swift_COMPILATION_MODE wholemodule)

target_link_libraries(swiftSynchronization
  PRIVATE
    swiftCore)

set(${PROJECT_NAME}_INSTALL_NESTED_SUBDIR "Install libraries under a platform and architecture subdirectory" ${Supplemental_INSTALL_NESTED_SUBDIR} CACHE BOOL "")
set(${PROJECT_NAME}_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${Supplemental_INSTALL_NESTED_SUBDIR}>:/${Supplemental_PLATFORM_SUBDIR}/${Supplemental_ARCH_SUBDIR}>" CACHE STRING "")
set(${PROJECT_NAME}_INSTALL_SWIFTMODULEDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${Supplemental_INSTALL_NESTED_SUBDIR}>:/${Supplemental_PLATFORM_SUBDIR}>" CACHE STRING "")

install(TARGETS swiftSynchronization
  EXPORT SwiftSupplementalTargets
  COMPONENT SwiftCore_runtime
  ARCHIVE DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${${PROJECT_NAME}_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
