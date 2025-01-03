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

set(CMAKE_C_VISIBILITY_PRESET "hidden")
set(CMAKE_CXX_VISIBILITY_PRESET "hidden")
set(CMAKE_VISIBILITY_INLINES_HIDDEN YES)

set(SwiftCore_CMAKE_MODULES_DIR "${CMAKE_SOURCE_DIR}/cmake/modules")
list(APPEND CMAKE_MODULE_PATH ${SwiftCore_CMAKE_MODULES_DIR})

include(CMakeWorkarounds)
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

# Hook point for vendor-specific extensions to the build system
# Allowed extension points:
#   - DefaultSettings.cmake
#   - Settings.cmake
set(SwiftCore_VENDOR_MODULE_DIR "${SwiftCore_CMAKE_MODULES_DIR}/vendor"
  CACHE FILEPATH "Location for private build system extension")

include(GNUInstallDirs)
include(AvailabilityMacros)
include(CompilerSettings)
include(DefaultSettings)
include(EmitSwiftInterface)
include(PlatformInfo)
include(gyb)
include(Plist)

include("${SwiftCore_VENDOR_MODULE_DIR}/Settings.cmake" OPTIONAL)

defaulted_option(SwiftCore_ENABLE_LIBRARY_EVOLUTION "Generate ABI resilient runtime libraries")

defaulted_option(SwiftCore_ENABLE_CRASH_REPORTER_CLIENT "Enable Apple CrashReporter integration")
defaulted_option(SwiftCore_ENABLE_OBJC_INTEROP "Enable runtime ObjC interop")
defaulted_option(SwiftCore_ENABLE_TYPE_PRINTING "Enable printing type names")
defaulted_option(SwiftCore_ENABLE_VECTOR_TYPES "Enable vector support")
defaulted_option(SwiftCore_ENABLE_REFLECTION "Enable runtime support for mirrors and reflection support")
defaulted_option(SwiftCore_ENABLE_COMMANDLINE_SUPPORT "Enable command line argument support")
defaulted_option(SwiftCore_ENABLE_RUNTIME_FUNCTION_COUNTERS "Enable runtime function counter support")
defaulted_option(SwiftCore_ENABLE_STDIN "Enable functions that use stdin support")
defaulted_option(SwiftCore_ENABLE_ENVIRONMENT "Enable environment variable support")
defaulted_option(SwiftCore_ENABLE_OVERRIDABLE_RETAIN_RELEASE "Enable override hooks for retain/release")
defaulted_option(SwiftCore_ENABLE_MALLOC_TYPE "Enable malloc type information")
defaulted_option(SwiftCore_ENABLE_RUNTIME_OS_VERSIONING "Enable runtime OS version detection")
defaulted_option(SwiftCore_ENABLE_STATIC_PRINT "Disable full print")
defaulted_option(SwiftCore_ENABLE_COMPACT_ABSOLUTE_FUNCTION_POINTERS "Resolve absolute function pointer as identity")
defaulted_option(SwiftCore_ENABLE_BACKDEPLOYMENT_SUPPORT "Add symbols for runtime backdeployment")
defaulted_option(SwiftCore_ENABLE_STDLIB_TRACING "Enable tracing in the runtime. Assumes the presence of os_log(3) and the os_signpost(3) API.")
option(SwiftCore_ENABLE_UNICODE_DATA "Include unicode data in Swift runtimes" ON)
option(SwiftCore_ENABLE_SHORT_MANGLING_LOOKUPS "Build with fast-path context descriptor lookups based on well-known short manglings." ON)

defaulted_option(SwiftCore_ENABLE_BACKTRACING "Enable backtracing runtime support")
defaulted_set(SwiftCore_BACKTRACER_PATH STRING "Set a fixed path to the Swift backtracer")

option(SwiftCore_ENABLE_CLOBBER_FREED_OBJECTS "" OFF)
option(SwiftCore_ENABLE_RUNTIME_LEAK_CHECKER "" OFF)
option(SwiftCore_ENABLE_INTERNAL_CHECKS "" OFF)

defaulted_set(SwiftCore_OBJECT_FORMAT STRING "Object format: ELF COFF")
defaulted_set(SwiftCore_THREADING_PACKAGE STRING "Threading Package: C11 Linux PThreads Win32")

# TODO: Most of these should be attached to specific targets rather than applied
#       globally. They can be applied as part of the library interface if they
#       affect the behavior of libraries that build against a given target.
add_compile_definitions(
  $<$<BOOL:${SwiftCore_ENABLE_OBJC_INTEROP}>:-DSWIFT_OBJC_INTEROP>
  $<$<BOOL:${SwiftCore_ENABLE_LIBRARY_EVOLUTION}>:-DSWIFT_LIBRARY_EVOLUTION>
  $<$<BOOL:${SwiftCore_ENABLE_CRASH_REPORTER_CLIENT}>:-DSWIFT_HAVE_CRASHREPORTERCLIENT>
  $<$<BOOL:${SwiftCore_ENABLE_VECTOR_TYPES}>:-DSWIFT_STDLIB_ENABLE_VECTOR_TYPES>
  $<$<BOOL:${SwiftCore_ENABLE_REFLECTION}>:-DSWIFT_ENABLE_REFLECTION>
  $<$<BOOL:${SwiftCore_ENABLE_RUNTIME_FUNCTION_COUNTERS}>:-DSWIFT_ENABLE_RUNTIME_FUNCTION_COUNTERS>
  $<$<BOOL:${SwiftCore_ENABLE_STDIN}>:-DSWIFT_STDLIB_HAS_STDIN> # core + stubs
  $<$<BOOL:${SwiftCore_ENABLE_RUNTIME_OS_VERSIONING}>:-DSWIFT_RUNTIME_OS_VERSIONING> # core + stubs
  $<$<BOOL:${SwiftCore_ENABLE_INTERNAL_CHECKS}>:-DINTERNAL_CHECKS_ENABLED> # core
  $<$<BOOL:${SwiftCore_ENABLE_STATIC_PRINT}>:-DSWIFT_STDLIB_STATIC_PRINT> # core + concurrency
  $<$<BOOL:${SwiftCore_ENABLE_UNICODE_DATA}>:-DSWIFT_STDLIB_ENABLE_UNICODE_DATA> # Stubs
  $<$<BOOL:${SwiftCore_ENABLE_ENVIRONMENT}>:-DSWIFT_STDLIB_HAS_ENVIRON> # Concurrency, runtime, shims, platform overlay
  $<$<BOOL:${SwiftCore_ENABLE_BACKDEPLOYMENT_SUPPORT}>:-DSWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT> # Concurrency, Compatibility override, magic symbols
  $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_THREADING_${SwiftCore_THREADING_PACKAGE}>
  $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_RUNTIME_ENABLE_LEAK_CHECKER=$<BOOL:${SwiftCore_ENABLE_RUNTIME_LEAK_CHECKER}>>
  $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_RUNTIME_CLOBBER_FREED_OBJECTS=$<BOOL:${SwiftCore_ENABLE_CLOBBER_FREED_OBJECTS}>>)

add_compile_options(
  $<$<COMPILE_LANGUAGE:CXX>:-fno-rtti>
  $<$<COMPILE_LANGUAGE:CXX>:-fno-exceptions>
  $<$<COMPILE_LANGUAGE:CXX>:-funwind-tables>
  "$<$<AND:$<COMPILE_LANGUAGE:Swift>,$<NOT:$<BOOL:${SwiftCore_ENABLE_OBJC_INTEROP}>>>:SHELL:-Xfrontend -disable-objc-interop>"
  "$<$<AND:$<COMPILE_LANGUAGE:Swift>,$<PLATFORM_ID:Windows>>:SHELL:-Xcc -Xclang -Xcc -fbuiltin-headers-in-system-modules>"
  $<$<AND:$<COMPILE_LANGUAGE:Swift>,$<BOOL:${SwiftCore_ENABLE_LIBRARY_EVOLUTION}>>:-enable-library-evolution>)

include_directories(include)

add_subdirectory(LLVMSupport)
add_subdirectory(SwiftShims/swift/shims)
add_subdirectory(Demangling)
add_subdirectory(Threading)
add_subdirectory(runtime)
add_subdirectory(stubs)
add_subdirectory(CommandLineSupport)
add_subdirectory(core)
