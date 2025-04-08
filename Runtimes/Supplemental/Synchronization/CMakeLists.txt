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

if(NOT PROJECT_IS_TOP_LEVEL)
  message(SEND_ERROR "Swift Synchronization must build as a standalone project")
endif()

set(${PROJECT_NAME}_SWIFTC_SOURCE_DIR #ToDo: What is this for, why would i need this for a supplemental lib?
  "${PROJECT_SOURCE_DIR}/../../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

find_package(SwiftCore)
include(gyb)

add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>
  $<$<COMPILE_LANGUAGE:Swift>:-enable-builtin-module> # Are these contradictory?
  $<$<COMPILE_LANGUAGE:Swift>:-strict-memory-safety>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature RawLayout>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature StaticExclusiveOnly>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature Extern>")

gyb_expand(Atomics/AtomicIntegers.swift.gyb Atomics/AtomicIntegers.swift)
gyb_expand(Atomics/AtomicStorage.swift.gyb Atomics/AtomicStorage.swift)

add_library(swift_Synchronization
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
  $<$<PLATFORM_ID:WASM>:Mutex/SpinLoopHint.swift> #ToDo is this right?
  $<$<PLATFORM_ID:WINDOWS>:Mutex/WindowsImpl.swift>)

set_target_properties(swift_Synchronization PROPERTIES
  Swift_MODULE_NAME _Synchronization
  Swift_COMPILATION_MODE wholemodule)

target_link_libraries(swift_Synchronization
  PRIVATE
    swiftCore)
