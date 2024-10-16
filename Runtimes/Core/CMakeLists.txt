# Notes:
#
# The Demangling library uses `#if SWIFT_HAVE_CRASHREPORTERCLIENT` while the
# runtime library uses `#ifdef SWIFT_HAVE_CRASHREPORTERCLIENT` to toggle that
# functionality. When building the demangling library, the macro should be set
# to 0 or 1 to indicate the presence of the crashreporter.
# When building the runtime library, the existence of the macro indicates the
# presence of the crashreporter.
#
# Runtime library pulls sources and headers from compiler sources (ThreadSanitizer)
# Demangling library pulls sources and headers from compiler sources (all)
#
#
# gyb pulls sources from compiler sources
#
# Stubs:
#   Pulls in headers from compiler
#     - include/swift/Basic
#     - include/swift/Runtime
#     - include/swift/Threading

# TODO:
# Platform support:
#   - Work on/Verify cross-compiling
#   - Work on/Verify Windows and Linux native builds
# Embedded
#   -- -Xfrontend -emit-empty-object-file
# Catalyst Support
#   -- Will need shadow invocations to generate swiftmodules for Swift parts
# Install *.abi.json, swiftdoc, and swiftsourceinfo

cmake_minimum_required(VERSION 3.26...3.29)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/modules")
project(SwiftCore LANGUAGES C CXX Swift VERSION 6.1)

# The Swift standard library is not intended for use as a sub-library as part of
# another project. It is tightly coupled with the compiler version.
if(NOT PROJECT_IS_TOP_LEVEL)
  message(FATAL_ERROR "Swift Core must build as a standalone project")
endif()

# FIXME: We should not need to refer back into the compiler sources. This is
#        needed by gyb, Demangling, Threading, the runtime (ThreadSanitizer.cpp)
set(SwiftCore_SWIFTC_SOURCE_DIR
  "${PROJECT_SOURCE_DIR}/../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

include(CompilerSettings)
include(DefaultSettings)
include(PlatformInfo)
include(gyb)

option(SwiftCore_ENABLE_CRASH_REPORTER_CLIENT "Enable Apple CrashReporter integration" OFF)
option(SwiftCore_ENABLE_OBJC_INTEROP "Enable runtime ObjC interop" OFF)
option(SwiftCore_ENABLE_TYPE_PRINTING "Enable printing type names" OFF)

option(SwiftCore_ENABLE_CLOBBER_FREED_OBJECTS "" ${SwiftCore_ENABLE_CLOBBER_FREED_OBJECTS_default})
option(SwiftCore_ENABLE_RUNTIME_LEAK_CHECKER "" ${SwiftCore_ENABLE_RIUNTIME_LEAK_CHECKER_default})
option(SwiftCore_ENABLE_REFLECTION "" OFF)

option(SwiftCore_ENABLE_BACKTRACING "Enable backtracing runtime support" OFF)
set(SwiftCore_BACKTRACER_PATH "" CACHE STRING "Set a fixed path to the Swift backtracer")

set(SwiftCore_OBJECT_FORMAT ${SwiftCore_OBJECT_FORMAT} CACHE STRING "Object format")

add_compile_definitions(
  $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_OBJC_INTEROP=$<BOOL:${SwiftCore_ENABLE_OBJC_INTEROP}>>
  $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_RUNTIME_ENABLE_LEAK_CHECKER=$<BOOL:${SwiftCore_ENABLE_RUNTIME_LEAK_CHECKER}>>
  $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_RUNTIME_CLOBBER_FREED_OBJECTS=$<BOOL:${SwiftCore_ENABLE_CLOBBER_FREED_OBJECTS}>>)

include_directories(include)

add_subdirectory(LLVMSupport)
add_subdirectory(SwiftShims/swift/shims)
add_subdirectory(Demangling)
add_subdirectory(Threading)
add_subdirectory(runtime)
add_subdirectory(stubs)
