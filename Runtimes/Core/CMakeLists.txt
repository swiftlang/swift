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

cmake_minimum_required(VERSION 3.29)

set(CMAKE_C_VISIBILITY_PRESET "hidden")
set(CMAKE_CXX_VISIBILITY_PRESET "hidden")
set(CMAKE_VISIBILITY_INLINES_HIDDEN YES)

set(CMAKE_POSITION_INDEPENDENT_CODE YES)

set(SwiftCore_CMAKE_MODULES_DIR "${CMAKE_SOURCE_DIR}/cmake/modules")
list(APPEND CMAKE_MODULE_PATH ${SwiftCore_CMAKE_MODULES_DIR})

include(CMakeWorkarounds)
# NOTE: always use the 3-component style as the expansion as
# `${PROJECT_VERSION}` will not extend this to the complete form and this can
# change the behaviour for comparison with non-SemVer compliant parsing. If
# possible, use the 4-version component as that is used to differentiate the
# builds of the runtime for Windows.
if($ENV{BUILD_NUMBER})
  # NOTE: SxS modules have a limit on each component being [0-65535].
  # https://learn.microsoft.com/en-us/windows/win32/sbscs/assembly-versions
  math(EXPR BUILD_NUMBER "$ENV{BUILD_NUMBER} % 65535")
  set(BUILD_NUMBER ".${BUILD_NUMBER}")
endif()
project(SwiftCore
  LANGUAGES C CXX Swift
  VERSION 6.1.0${BUILD_NUMBER})

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
include(CheckSymbolExists)
include(CheckIncludeFileCXX)
include(CompilerSettings)
include(DefaultSettings)
include(EmitSwiftInterface)
include(PlatformInfo)
include(gyb)
include(ResourceEmbedding)
include(CatalystSupport)
include(AvailabilityMacros)

check_symbol_exists("asl_log" "asl.h" SwiftCore_HAS_ASL)
check_symbol_exists("dladdr" "dlfcn.h" SwiftCore_HAS_DLADDR)
check_symbol_exists("dlsym" "dlfcn.h" SwiftCore_HAS_DLSYM)
check_symbol_exists("os_trace" "os/trace.h" SwiftCore_HAS_OS_TRACE)

check_include_file_cxx("clocale" SwiftCore_HAS_LOCALE)

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
defaulted_option(SwiftCore_ENABLE_RUNTIME_OS_VERSIONING "Enable runtime OS version detection")
defaulted_option(SwiftCore_ENABLE_STATIC_PRINT "Disable full print")
defaulted_option(SwiftCore_ENABLE_COMPACT_ABSOLUTE_FUNCTION_POINTERS "Resolve absolute function pointer as identity")
defaulted_option(SwiftCore_ENABLE_BACKDEPLOYMENT_SUPPORT "Add symbols for runtime backdeployment")
defaulted_option(SwiftCore_ENABLE_STDLIB_TRACING "Enable tracing in the runtime. Assumes the presence of os_log(3) and the os_signpost(3) API.")
defaulted_option(SwiftCore_ENABLE_CONCURRENCY "Enable Concurrency runtime support")
defaulted_set(SwiftCore_CONCURRENCY_GLOBAL_EXECUTOR STRING "Default Concurrency global executor implementation")
option(SwiftCore_ENABLE_UNICODE_DATA "Include unicode data in Swift runtimes" ON)
option(SwiftCore_ENABLE_SHORT_MANGLING_LOOKUPS "Build with fast-path context descriptor lookups based on well-known short manglings." ON)
option(SwiftCore_ENABLE_FILESYSTEM_SUPPORT "Build for systems that have a filesystem" ON)
option(SwiftCore_ENABLE_OS_TRACE_LAZY_INIT "Use os_trace call to check if lazy init has been completed before making os_signpost calls." ${SwiftCore_HAS_OS_TRACE})
option(SwiftCore_HAS_DARWIN_LIBMALLOC "Use Darwin malloc features" ${APPLE})

defaulted_option(SwiftCore_ENABLE_BACKTRACING "Enable backtracing runtime support")
defaulted_set(SwiftCore_BACKTRACER_PATH STRING "Set a fixed path to the Swift backtracer")
defaulted_option(SwiftCore_ENABLE_FATALERROR_BACKTRACE "Build stdlib fatalError with backtrace output")
defaulted_option(SwiftCore_ENABLE_PRESPECIALIZATION "Enable generic metadata prespecialization")
defaulted_option(SwiftCore_ENABLE_STRICT_AVAILABILITY "Enable strict availability; this will cause things to break at desk or in CI if the host OS is a lower version than some `@availability` annotations in the runtime code")

option(SwiftCore_ENABLE_CLOBBER_FREED_OBJECTS "" OFF)
option(SwiftCore_ENABLE_RUNTIME_LEAK_CHECKER "" OFF)
option(SwiftCore_ENABLE_INTERNAL_CHECKS "" OFF)
set(SwiftCore_OPTIMIZATION_REMARKS OFF CACHE STRING "Set opt-remark format")

defaulted_set(SwiftCore_OBJECT_FORMAT STRING "Object format: ELF COFF")
defaulted_set(SwiftCore_THREADING_PACKAGE STRING "Threading Package: C11 Linux PThreads Win32")

option(SwiftCore_INSTALL_NESTED_SUBDIR "Install libraries under a platform and architecture subdirectory" ON)
set(SwiftCore_INSTALL_LIBDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${SwiftCore_INSTALL_NESTED_SUBDIR}>:/${SwiftCore_PLATFORM_SUBDIR}/${SwiftCore_ARCH_SUBDIR}>")
set(SwiftCore_INSTALL_SWIFTMODULEDIR "${CMAKE_INSTALL_LIBDIR}/swift$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:_static>$<$<BOOL:${SwiftCore_INSTALL_NESTED_SUBDIR}>:/${SwiftCore_PLATFORM_SUBDIR}>")

option(SwiftCore_ENABLE_ONONESUPPORT "Enable Onone Support" YES)

# TODO: Most of these should be attached to specific targets rather than applied
#       globally. They can be applied as part of the library interface if they
#       affect the behavior of libraries that build against a given target.
add_compile_definitions(
  $<$<BOOL:${SwiftCore_ENABLE_OBJC_INTEROP}>:-DSWIFT_OBJC_INTEROP>
  $<$<BOOL:${SwiftCore_ENABLE_LIBRARY_EVOLUTION}>:-DSWIFT_LIBRARY_EVOLUTION>
  $<$<BOOL:${SwiftCore_ENABLE_CRASH_REPORTER_CLIENT}>:-DSWIFT_HAVE_CRASHREPORTERCLIENT>
  $<$<BOOL:${SwiftCore_ENABLE_REFLECTION}>:-DSWIFT_ENABLE_REFLECTION>
  $<$<BOOL:${SwiftCore_ENABLE_RUNTIME_FUNCTION_COUNTERS}>:-DSWIFT_ENABLE_RUNTIME_FUNCTION_COUNTERS>
  $<$<BOOL:${SwiftCore_ENABLE_STDIN}>:-DSWIFT_STDLIB_HAS_STDIN> # core + stubs
  $<$<BOOL:${SwiftCore_ENABLE_RUNTIME_OS_VERSIONING}>:-DSWIFT_RUNTIME_OS_VERSIONING> # core + stubs
  $<$<BOOL:${SwiftCore_ENABLE_INTERNAL_CHECKS}>:-DINTERNAL_CHECKS_ENABLED> # core
  $<$<BOOL:${SwiftCore_ENABLE_STATIC_PRINT}>:-DSWIFT_STDLIB_STATIC_PRINT> # core + concurrency
  $<$<BOOL:${SwiftCore_ENABLE_UNICODE_DATA}>:-DSWIFT_STDLIB_ENABLE_UNICODE_DATA> # Stubs
  $<$<BOOL:${SwiftCore_ENABLE_ENVIRONMENT}>:-DSWIFT_STDLIB_HAS_ENVIRON> # Concurrency, runtime, shims, platform overlay
  $<$<BOOL:${SwiftCore_ENABLE_BACKDEPLOYMENT_SUPPORT}>:-DSWIFT_STDLIB_SUPPORT_BACK_DEPLOYMENT> # Concurrency, Compatibility override, magic symbols
  $<$<BOOL:${SwiftCore_HAS_DARWIN_LIBMALLOC}>:-DSWIFT_STDLIB_HAS_DARWIN_LIBMALLOC> # Anything that includes include/swift/Runtime/Config.h
  $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_THREADING_${SwiftCore_THREADING_PACKAGE}>
  $<$<COMPILE_LANGUAGE:C,CXX>:-DSWIFT_RUNTIME_ENABLE_LEAK_CHECKER=$<BOOL:${SwiftCore_ENABLE_RUNTIME_LEAK_CHECKER}>>
  $<$<BOOL:${SwiftCore_ENABLE_CLOBBER_FREED_OBJECTS}>:-DSWIFT_RUNTIME_CLOBBER_FREED_OBJECTS>)

add_compile_options(
  $<$<COMPILE_LANGUAGE:CXX>:-fno-rtti>
  $<$<COMPILE_LANGUAGE:CXX>:-fno-exceptions>
  $<$<COMPILE_LANGUAGE:CXX>:-funwind-tables>)

# LNK4049: symbol 'symbol' defined in 'filename.obj' is imported
# LNK4286: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj'
# LNK4217: symbol 'symbol' defined in 'filename_1.obj' is imported by 'filename_2.obj' in function 'function'
#
# We cannot selectively filter the linker warnings as we do not use the MSVC
# frontned and `clang-cl` (and `clang`) currently do not support `/WX:nnnn`. As
# a compromise, treat all linker warnings as errors.
add_link_options($<$<PLATFORM_ID:Windows>:LINKER:/WX>)

add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>
  "$<$<COMPILE_LANGUAGE:Swift>:-nostdlibimport>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-library-level api>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-runtime-compatibility-version none>"
  "$<$<COMPILE_LANGUAGE:Swift>:-disable-autolinking-runtime-compatibility-dynamic-replacements>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-autolinking-runtime-compatibility-concurrency>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-strict-memory-safety>"
  "$<$<AND:$<BOOL:${SwiftCore_OPTIMIZATION_REMARKS}>,$<COMPILE_LANGUAGE:Swift>>:-save-optimization-record=${SwiftCore_OPTIMIZATION_REMARKS}>"
  "$<$<COMPILE_LANGUAGE:Swift>:-warn-implicit-overrides>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-lexical-lifetimes=false>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-concurrency-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -disable-implicit-string-processing-module-import>"
  "$<$<COMPILE_LANGUAGE:Swift>:-no-link-objc-runtime>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enforce-exclusivity=unchecked>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -enable-ossa-modules>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -empty-abi-descriptor>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xfrontend -target-min-inlining-version -Xfrontend min>"
  "$<$<AND:$<NOT:$<BOOL:${SwiftCore_ENABLE_OBJC_INTEROP}>>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xfrontend -disable-objc-interop>"
  "$<$<AND:$<PLATFORM_ID:Windows>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xcc -Xclang -Xcc -fbuiltin-headers-in-system-modules>"
  "$<$<AND:$<BOOL:${SwiftCore_ENABLE_LIBRARY_EVOLUTION}>,$<COMPILE_LANGUAGE:Swift>>:-enable-library-evolution>"
  "$<$<AND:$<BOOL:${SwiftCore_ENABLE_PRESPECIALIZATION}>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xfrontend -prespecialize-generic-metadata>")

include(ExperimentalFeatures)

include_directories(include)

add_subdirectory(LLVMSupport)
add_subdirectory(SwiftShims/swift/shims)
add_subdirectory(Demangling)
add_subdirectory(Threading)
add_subdirectory(runtime)
add_subdirectory(stubs)
add_subdirectory(CommandLineSupport)
add_subdirectory(core)
if(SwiftCore_ENABLE_ONONESUPPORT)
  add_subdirectory(SwiftOnoneSupport)
endif()
if(SwiftCore_ENABLE_CONCURRENCY)
  add_subdirectory(Concurrency)
endif()

# Inter-project install info
export(EXPORT SwiftCoreTargets
  FILE "cmake/SwiftCore/SwiftCoreTargets.cmake")
install(EXPORT SwiftCoreTargets
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftCore"
  FILE "SwiftCoreTargets.cmake"
  COMPONENT SwiftCore_cmake)
include(CMakePackageConfigHelpers)
configure_package_config_file("${CMAKE_CURRENT_SOURCE_DIR}/cmake/interface/SwiftCoreConfig.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftCore/SwiftCoreConfig.cmake"
  INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftCore")
write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftCore/SwiftCoreConfigVersion.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY ExactVersion)
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftCore/SwiftCoreConfig.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/SwiftCore/SwiftCoreConfigVersion.cmake"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/SwiftCore")
