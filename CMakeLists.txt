cmake_minimum_required(VERSION 2.8.12)

# Add path for custom CMake modules.
list(APPEND CMAKE_MODULE_PATH
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

include(SwiftBuildType)

#
# User-configurable options.
#
# Instead of invoking CMake directly and configuring these options manually,
# consider using build-script instead.
#

set(SWIFT_STDLIB_BUILD_TYPE "${CMAKE_BUILD_TYPE}" CACHE STRING
    "Build type for the Swift standard library and SDK overlays [Debug, RelWithDebInfo, Release, MinSizeRel]")
set_property(CACHE SWIFT_STDLIB_BUILD_TYPE PROPERTY
    STRINGS "Debug" "RelWithDebInfo" "Release" "MinSizeRel")

is_build_type_optimized("${SWIFT_STDLIB_BUILD_TYPE}" swift_optimized)
if(swift_optimized)
  set(SWIFT_STDLIB_ASSERTIONS_default FALSE)
else()
  set(SWIFT_STDLIB_ASSERTIONS_default TRUE)
endif()
option(SWIFT_STDLIB_ASSERTIONS
    "Enable internal checks for the Swift standard library (useful for debugging the library itself, does not affect checks required for safety)"
    "${SWIFT_STDLIB_ASSERTIONS_default}")

option(SWIFT_SERIALIZE_STDLIB_UNITTEST
    "Compile the StdlibUnittest module with -sil-serialize-all to increase the test coverage for the optimizer"
    FALSE)

option(SWIFT_STDLIB_SIL_DEBUGGING
    "Compile the Swift standard library with -gsil to enable debugging and profiling on SIL level"
    FALSE)

option(SWIFT_BUILD_TOOLS
    "Build the Swift compiler and other tools"
    TRUE)

option(SWIFT_BUILD_STDLIB
    "Build the Swift standard library (independent of the SDK headers)"
    TRUE)

option(SWIFT_BUILD_SDK_OVERLAY
    "Build Swift SDK overlay"
    TRUE)

option(SWIFT_BUILD_STATIC_STDLIB
    "Build static variants of the Swift standard library and SDK overlay"
    FALSE)

option(SWIFT_BUILD_PERF_TESTSUITE
    "Create targets for swift performance benchmarks."
    FALSE)

option(SWIFT_INCLUDE_TESTS "Create targets for building/running tests." TRUE)

option(SWIFT_INCLUDE_DOCS
    "Create targets for building docs."
    TRUE)

option(SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER
       "Use the host compiler and not the internal clang to build the swift runtime"
       FALSE)

set(SWIFT_ANALYZE_CODE_COVERAGE FALSE CACHE STRING
    "Build Swift with code coverage instrumenting enabled [FALSE, NOT-MERGED, MERGED]")
set_property(CACHE SWIFT_ANALYZE_CODE_COVERAGE PROPERTY
    STRINGS FALSE "NOT-MERGED" "MERGED")

set(SWIFT_VERSION "3.0" CACHE STRING
    "The user-visible version of the Swift compiler")
set(SWIFT_VENDOR "" CACHE STRING
    "The vendor name of the Swift compiler")
set(SWIFT_COMPILER_VERSION "" CACHE STRING
    "The internal version of the Swift compiler")
set(CLANG_COMPILER_VERSION "" CACHE STRING
    "The internal version of the Clang compiler")

# Indicate whether Swift should attempt to use the gold linker.
# This is not used on Darwin.
set(SWIFT_ENABLE_GOLD_LINKER FALSE CACHE BOOL
    "Enable using the gold linker when available")

set(_SWIFT_KNOWN_INSTALL_COMPONENTS
    "autolink-driver;compiler;clang-builtin-headers;clang-resource-dir-symlink;clang-builtin-headers-in-clang-resource-dir;stdlib;stdlib-experimental;sdk-overlay;editor-integration;tools;testsuite-tools;dev;license;sourcekit-xpc-service;sourcekit-inproc")

# Set the SWIFT_INSTALL_COMPONENTS variable to the default value if it is not passed in via -D
set(SWIFT_INSTALL_COMPONENTS "${_SWIFT_KNOWN_INSTALL_COMPONENTS}" CACHE STRING
    "A semicolon-separated list of components to install ${_SWIFT_KNOWN_INSTALL_COMPONENTS}")
# Components are disjoint set of files that can be installed by Swift.  The
# components would approximately correspond to packages in a Debian-style Linux
# packaging.  The following components are defined:
#
# * autolink-driver -- the Swift driver support tools
# * compiler -- the Swift compiler and (on supported platforms) the REPL.
# * clang-builtin-headers -- install a copy of Clang builtin headers under
#   'lib/swift/clang'.  This is useful when Swift compiler is installed in
#   isolation.
# * clang-resource-dir-symlink -- install a symlink to the Clang resource
#   directory (which contains builtin headers) under 'lib/swift/clang'.  This is
#   useful when Clang and Swift are installed side-by-side.
# * stdlib -- the Swift standard library.
# * stdlib-experimental -- the Swift standard library module for experimental
#   APIs.
# * sdk-overlay -- the Swift SDK overlay.
# * editor-integration -- scripts for Swift integration in IDEs other than
#   Xcode;
# * tools -- tools (other than the compiler) useful for developers writing
#   Swift code.
# * testsuite-tools -- extra tools required to run the Swift testsuite.
# * dev -- headers and libraries required to use Swift compiler as a library.

set(SWIFT_SDKS "" CACHE STRING
    "If non-empty, limits building target binaries only to specified SDKs (despite other SDKs being available)")

set(SWIFT_PRIMARY_VARIANT_SDK "" CACHE STRING
    "Primary SDK for target binaries")
set(SWIFT_PRIMARY_VARIANT_ARCH "" CACHE STRING
    "Primary arch for target binaries")

set(SWIFT_NATIVE_LLVM_TOOLS_PATH "" CACHE STRING
    "Path to the directory that contains LLVM tools that are executable on the build machine")

set(SWIFT_NATIVE_CLANG_TOOLS_PATH "" CACHE STRING
    "Path to the directory that contains Clang tools that are executable on the build machine")

set(SWIFT_NATIVE_SWIFT_TOOLS_PATH "" CACHE STRING
    "Path to the directory that contains Swift tools that are executable on the build machine")

option(SWIFT_ENABLE_LTO
  "If set to true, build the swift compiler with link time optimization enabled" FALSE)

# The following only works with the Ninja generator in CMake >= 3.0.
set(SWIFT_PARALLEL_LINK_JOBS "" CACHE STRING
  "Define the maximum number of linker jobs for swift.")
set(SWIFT_ANDROID_NDK_PATH "" CACHE STRING
  "Path to the directory that contains the Android NDK tools that are executable on the build machine")
set(SWIFT_ANDROID_NDK_GCC_VERSION "" CACHE STRING
  "The GCC version to use when building for Android. Currently only 4.9 is supported.")
set(SWIFT_ANDROID_SDK_PATH "" CACHE STRING
  "Path to the directory that contains the Android SDK tools that will be passed to the swiftc frontend")
set(SWIFT_ANDROID_ICU_UC "" CACHE STRING
  "Path to a directory containing libicuuc.so")
set(SWIFT_ANDROID_ICU_UC_INCLUDE "" CACHE STRING
  "Path to a directory containing headers for libicuuc")
set(SWIFT_ANDROID_ICU_I18N "" CACHE STRING
  "Path to a directory containing libicui18n.so")
set(SWIFT_ANDROID_ICU_I18N_INCLUDE "" CACHE STRING
  "Path to a directory containing headers libicui18n")

#
# User-configurable Darwin-specific options.
#

option(SWIFT_EMBED_BITCODE_SECTION
    "If non-empty, embeds LLVM bitcode binary sections in the standard library and overlay binaries for supported platforms"
    FALSE)

option(SWIFT_RUNTIME_CRASH_REPORTER_CLIENT
    "Whether to enable CrashReporter integration"
    FALSE)

set(SWIFT_DARWIN_XCRUN_TOOLCHAIN "XcodeDefault" CACHE STRING
    "The name of the toolchain to pass to 'xcrun'")

set(SWIFT_DARWIN_STDLIB_INSTALL_NAME_DIR "@rpath" CACHE STRING
    "The directory of the install_name for standard library dylibs")

set(SWIFT_DARWIN_DEPLOYMENT_VERSION_OSX "10.9" CACHE STRING
    "Minimum deployment target version for OS X")

set(SWIFT_DARWIN_DEPLOYMENT_VERSION_IOS "7.0" CACHE STRING
    "Minimum deployment target version for iOS")

set(SWIFT_DARWIN_DEPLOYMENT_VERSION_TVOS "9.0" CACHE STRING
    "Minimum deployment target version for tvOS")

set(SWIFT_DARWIN_DEPLOYMENT_VERSION_WATCHOS "2.0" CACHE STRING
    "Minimum deployment target version for watchOS")

#
# User-configurable debugging options.
#

option(SWIFT_AST_VERIFIER
    "Enable the AST verifier in the built compiler, and run it on every compilation"
    TRUE)

option(SWIFT_SIL_VERIFY_ALL
    "Run SIL verification after each transform when building Swift files in the build process"
    FALSE)

option(SWIFT_EMIT_SORTED_SIL_OUTPUT
    "Sort SIL output by name to enable diffing of output"
    FALSE)

if(SWIFT_STDLIB_ASSERTIONS)
  set(SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS_default TRUE)
else()
  set(SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS_default FALSE)
endif()

option(SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS
    "Overwrite memory for deallocated Swift objects"
    "${SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS_default}")

#
# User-configurable experimental options.  Do not use in production builds.
#

set(SWIFT_EXPERIMENTAL_EXTRA_FLAGS "" CACHE STRING
    "Extra flags to pass when compiling swift files.  Use this option *only* for one-off experiments")

set(SWIFT_EXPERIMENTAL_EXTRA_REGEXP_FLAGS "" CACHE STRING
  "A list of [module_regexp1;flags1;module_regexp2;flags2,...] which can be used to apply specific flags to modules that match a cmake regexp. It always applies the first regexp that matches.")

set(SWIFT_EXPERIMENTAL_EXTRA_NEGATIVE_REGEXP_FLAGS "" CACHE STRING
    "A list of [module_regexp1;flags1;module_regexp2;flags2,...] which can be used to apply specific flags to modules that do not match a cmake regexp. It always applies the first regexp that does not match. The reason this is necessary is that cmake does not provide negative matches in the regex. Instead you have to use NOT in the if statement requiring a separate variable.")

option(SWIFT_RUNTIME_ENABLE_LEAK_CHECKER
  "Should the runtime be built with support for non-thread-safe leak detecting entrypoints"
  FALSE)

option(SWIFT_STDLIB_ENABLE_RESILIENCE
    "Build the standard libraries and overlays with resilience enabled; see docs/LibraryEvolution.rst"
    FALSE)

option(SWIFT_STDLIB_ENABLE_REFLECTION_METADATA
    "Build the standard libraries and overlays with remote reflection metadata; see docs/proposals/RemoteMirrors.rst"
    FALSE)

option(SWIFT_STDLIB_SIL_SERIALIZE_ALL
    "Build the standard libraries and overlays serializing all method bodies"
    TRUE)

if(SWIFT_SERIALIZE_STDLIB_UNITTEST AND SWIFT_STDLIB_ENABLE_RESILIENCE)
  message(WARNING "Ignoring SWIFT_SERIALIZE_STDLIB_UNITTEST because SWIFT_STDLIB_ENABLE_RESILIENCE is set")
  set(SWIFT_SERIALIZE_STDLIB_UNITTEST FALSE)
endif()

if(SWIFT_STDLIB_SIL_SERIALIZE_ALL AND SWIFT_STDLIB_ENABLE_RESILIENCE)
  message(WARNING "Ignoring SWIFT_STDLIB_SIL_SERIALIZE_ALL because SWIFT_STDLIB_ENABLE_RESILIENCE is set")
  set(SWIFT_STDLIB_SIL_SERIALIZE_ALL FALSE)
endif()

#
# End of user-configurable options.
#

set(SWIFT_BUILT_STANDALONE FALSE)
if("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_SOURCE_DIR}")
  set(SWIFT_BUILT_STANDALONE TRUE)
endif()

if(SWIFT_BUILT_STANDALONE)
  project(Swift C CXX ASM)
endif()

if("${CMAKE_SYSTEM_NAME}" STREQUAL "")
  message(FATAL_ERROR "CMAKE_SYSTEM_NAME is empty!")
endif()
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  set(SWIFT_BUILD_SOURCEKIT_default TRUE)
else()
  set(SWIFT_BUILD_SOURCEKIT_default FALSE)
endif()
option(SWIFT_BUILD_SOURCEKIT
    "Build SourceKit"
    ${SWIFT_BUILD_SOURCEKIT_default})

#
# Include CMake modules
#

include(CheckCXXSourceRuns)
include(CMakeParseArguments)
include(SwiftTranslateFlag)
include(SwiftHandleGybSources)
include(SwiftSetIfArchBitness)
include(AddSwift)
include(SwiftConfigureSDK)
include(SwiftInstallComponents)
include(SwiftGetConfigurationTypes)
include(SwiftApplyXcodeSubstitutions)
include(SwiftList)

swift_configure_install_components("${SWIFT_INSTALL_COMPONENTS}")

if("${CMAKE_VERSION}" VERSION_LESS "3.0")
  set(SWIFT_CMAKE_HAS_GENERATOR_EXPRESSIONS FALSE)
else()
  set(SWIFT_CMAKE_HAS_GENERATOR_EXPRESSIONS TRUE)
endif()

# lipo is used to create universal binaries.
find_program(LIPO "lipo")

if("${SWIFT_NATIVE_LLVM_TOOLS_PATH}" STREQUAL "")
  set(SWIFT_CROSS_COMPILING FALSE)
else()
  set(SWIFT_CROSS_COMPILING TRUE)
endif()

# Reset CMAKE_SYSTEM_PROCESSOR if not cross-compiling.
# CMake refuses to use `uname -m` on OS X
# http://public.kitware.com/Bug/view.php?id=10326
if(NOT CMAKE_CROSSCOMPILING AND CMAKE_SYSTEM_PROCESSOR STREQUAL "i386")
  execute_process(
      COMMAND "uname" "-m"
      OUTPUT_VARIABLE CMAKE_SYSTEM_PROCESSOR
      OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()

include(SwiftSharedCMakeConfig)

# Support building Swift as a standalone project, using LLVM as an
# external library.
if(SWIFT_BUILT_STANDALONE)
  swift_common_standalone_build_config(SWIFT SWIFT_CROSS_COMPILING)
else()
  swift_common_unified_build_config(SWIFT)
endif()

if(NOT EXISTS "${CLANG_MAIN_INCLUDE_DIR}/clang/AST/Decl.h")
  message(FATAL_ERROR "Clang is missing from llvm/tools subdirectory.")
endif()

# This could be computed using ${CMAKE_CFG_INTDIR} if we want to link Swift
# against a matching LLVM build configuration.  However, we usually want to be
# flexible and allow linking a debug Swift against optimized LLVM.
set(LLVM_RUNTIME_OUTPUT_INTDIR "${LLVM_BINARY_DIR}")
set(LLVM_LIBRARY_OUTPUT_INTDIR "${LLVM_LIBRARY_DIR}")

set(SWIFT_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
set(SWIFT_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}")

set(SWIFT_RUNTIME_OUTPUT_INTDIR "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bin")
set(SWIFT_LIBRARY_OUTPUT_INTDIR "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib")
if("${SWIFT_NATIVE_SWIFT_TOOLS_PATH}" STREQUAL "")
  set(SWIFT_NATIVE_SWIFT_TOOLS_PATH "${SWIFT_RUNTIME_OUTPUT_INTDIR}")
endif()

# We'll need this once we have generated headers
include_directories(BEFORE
  ${CMAKE_CURRENT_BINARY_DIR}/include
  ${CMAKE_CURRENT_SOURCE_DIR}/include
  )

# A convenience pattern to match Darwin platforms. Example:
#  if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
#     ...
#  endif()
set(SWIFT_DARWIN_VARIANTS "^(macosx|iphoneos|iphonesimulator|appletvos|appletvsimulator|watchos|watchsimulator)")

# Configuration flags passed to all of our invocations of gyb.  Try to
# avoid making up new variable names here if you can find a CMake
# variable that will do the job.
set(SWIFT_GYB_FLAGS
    "-DunicodeGraphemeBreakPropertyFile=${SWIFT_SOURCE_DIR}/utils/UnicodeData/GraphemeBreakProperty.txt"
    "-DunicodeGraphemeBreakTestFile=${SWIFT_SOURCE_DIR}/utils/UnicodeData/GraphemeBreakTest.txt")

# Directory to use as the Clang module cache when building Swift source files.
set(SWIFT_MODULE_CACHE_PATH
    "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/module-cache")

# Xcode: use libc++ and c++11 using proper build settings.
if(XCODE)
  swift_common_xcode_cxx_config()
endif()

function(check_working_std_regex result_var_name)
  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    # Apple operating systems use libc++, which has a working std::regex.
    set("${result_var_name}" TRUE PARENT_SCOPE)
  else()
    # libstdc++ 4.8 has an incomplete std::regex implementation, and crashes
    # on many regexes.
    # libstdc++ 4.9 works.
    set(std_regex_test_source
"
#include <regex>
const std::regex broken_regex{
  \"([a]+)\",
  std::regex::ECMAScript | std::regex::nosubs};

int main() {}
")

    check_cxx_source_runs("${std_regex_test_source}" result)
    if (result)
      set("${result_var_name}" TRUE PARENT_SCOPE)
    else()
      set("${result_var_name}" FALSE PARENT_SCOPE)
    endif()
  endif()
endfunction()
check_working_std_regex(SWIFT_HAVE_WORKING_STD_REGEX)

#
# Enable additional warnings.
#
swift_common_cxx_warnings()

#
# Configure SDKs.
#

function(is_sdk_requested name result_var_name)
  if("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "${name}")
    set("${result_var_name}" "TRUE" PARENT_SCOPE)
  else()
    if("${SWIFT_SDKS}" STREQUAL "")
      set("${result_var_name}" "TRUE" PARENT_SCOPE)
    else()
      list(FIND SWIFT_SDKS "${name}" sdk_index)
      if(${sdk_index} EQUAL -1)
        set("${result_var_name}" "FALSE" PARENT_SCOPE)
      else()
        set("${result_var_name}" "TRUE" PARENT_SCOPE)
      endif()
    endif()
  endif()
endfunction()

# FIXME: separate the notions of SDKs used for compiler tools and target
# binaries.
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  set(CMAKE_EXECUTABLE_FORMAT "ELF")

  set(SWIFT_HOST_VARIANT "linux" CACHE STRING
      "Deployment OS for Swift host tools (the compiler) [linux].")

  set(SWIFT_HOST_VARIANT_SDK "LINUX")
  set(SWIFT_PRIMARY_VARIANT_SDK_default "LINUX")

  # FIXME: This will not work while trying to cross-compile.
  if("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
    set(SWIFT_HOST_VARIANT_ARCH "x86_64")
    set(SWIFT_PRIMARY_VARIANT_ARCH_default "x86_64")

    if("${SWIFT_ANDROID_NDK_PATH}" STREQUAL "")
      set(swift_can_crosscompile_stdlib FALSE)
    else()
      set(swift_can_crosscompile_stdlib TRUE)
    endif()

    is_sdk_requested(LINUX swift_build_linux)
    if(swift_build_linux)
      configure_sdk_unix(LINUX "Linux" "linux" "linux" "x86_64" "x86_64-unknown-linux-gnu")
      set(SWIFT_PRIMARY_VARIANT_SDK_default "LINUX")
      set(SWIFT_PRIMARY_VARIANT_ARCH_default "x86_64")
    endif()

    is_sdk_requested(ANDROID swift_build_android)
    if(swift_build_android AND ${swift_can_crosscompile_stdlib})
      configure_sdk_unix(ANDROID "Android" "android" "android" "armv7" "armv7-none-linux-androideabi")
      # This must be set, as variables such as "${SWIFT_SDK_${sdk}_PATH}" are
      # referenced in several other locations.
      set(SWIFT_SDK_ANDROID_PATH "${SWIFT_ANDROID_SDK_PATH}")

      set(SWIFT_PRIMARY_VARIANT_SDK_default "ANDROID")
      set(SWIFT_PRIMARY_VARIANT_ARCH_default "armv7")
    endif()

  # FIXME: This only matches ARMv6l (by far the most common variant).
  elseif("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "armv6l")
    configure_sdk_unix(LINUX "Linux" "linux" "linux" "armv6" "armv6-unknown-linux-gnueabihf")
    set(SWIFT_HOST_VARIANT_ARCH "armv6")
    set(SWIFT_PRIMARY_VARIANT_ARCH_default "armv6")
  # FIXME: This only matches ARMv7l (by far the most common variant).
  elseif("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "armv7l")
    configure_sdk_unix(LINUX "Linux" "linux" "linux" "armv7" "armv7-unknown-linux-gnueabihf")
    set(SWIFT_HOST_VARIANT_ARCH "armv7")
    set(SWIFT_PRIMARY_VARIANT_ARCH_default "armv7")
  elseif("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "aarch64")
    configure_sdk_unix(LINUX "Linux" "linux" "linux" "aarch64" "aarch64-unknown-linux-gnu")
    set(SWIFT_HOST_VARIANT_ARCH "aarch64")
    set(SWIFT_PRIMARY_VARIANT_ARCH_default "aarch64")
  elseif("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "ppc64")
    configure_sdk_unix(LINUX "Linux" "linux" "linux" "powerpc64" "powerpc64-unknown-linux-gnu")
    set(SWIFT_HOST_VARIANT_ARCH "powerpc64")
    set(SWIFT_PRIMARY_VARIANT_ARCH_default "powerpc64")
  elseif("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "ppc64le")
    configure_sdk_unix(LINUX "Linux" "linux" "linux" "powerpc64le" "powerpc64le-unknown-linux-gnu")
    set(SWIFT_HOST_VARIANT_ARCH "powerpc64le")
    set(SWIFT_PRIMARY_VARIANT_ARCH_default "powerpc64le")
  else()
    message(FATAL_ERROR "Unknown or unsupported architecture: ${CMAKE_SYSTEM_PROCESSOR}")
  endif()

elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "FreeBSD")
  # FIXME: Using the host OS version won't produce correct results for
  # cross-compilation.
  string(REPLACE "[-].*" "" FREEBSD_SYSTEM_VERSION ${CMAKE_SYSTEM_VERSION})
  message(STATUS "FreeBSD Version: ${FREEBSD_SYSTEM_VERSION}")
  configure_sdk_unix(FREEBSD "FreeBSD" "freebsd" "freebsd" "x86_64"
    "x86_64-unknown-freebsd${FREEBSD_SYSTEM_VERSION}")

  set(CMAKE_EXECUTABLE_FORMAT "ELF")

  set(SWIFT_HOST_VARIANT "freebsd" CACHE STRING
      "Deployment OS for Swift host tools (the compiler) [freebsd].")

  set(SWIFT_HOST_VARIANT_SDK "FREEBSD")
  set(SWIFT_HOST_VARIANT_ARCH "x86_64")

  set(SWIFT_PRIMARY_VARIANT_SDK_default "FREEBSD")
  set(SWIFT_PRIMARY_VARIANT_ARCH_default "x86_64")
elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "CYGWIN")
  configure_sdk_unix(CYGWIN "Cygwin" "windows" "cygwin" "x86_64" "x86_64-unknown-windows-cygnus")

#  set(CMAKE_EXECUTABLE_FORMAT "ELF")

  set(SWIFT_HOST_VARIANT "windows" CACHE STRING
      "Deployment OS for Swift host tools (the compiler) [windows].")

  set(SWIFT_HOST_VARIANT_SDK "CYGWIN")
  set(SWIFT_HOST_VARIANT_ARCH "x86_64")
  
  set(SWIFT_PRIMARY_VARIANT_SDK_default "CYGWIN")
  set(SWIFT_PRIMARY_VARIANT_ARCH_default "x86_64")
elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  # Set defaults.

  set(SWIFT_HOST_VARIANT "macosx" CACHE STRING
      "Deployment OS for Swift host tools (the compiler) [macosx, iphoneos].")

  set(SWIFT_HOST_VARIANT_SDK "OSX" CACHE STRING
      "Deployment sdk for Swift host tools (the compiler).")
  set(SWIFT_HOST_VARIANT_ARCH "x86_64" CACHE STRING
      "Deployment arch for Swift host tools (the compiler).")

  # Display Xcode toolchain version. 
  # The SDK configuration below prints each SDK version.
  execute_process(
    COMMAND "xcodebuild" "-version"
    OUTPUT_VARIABLE xcode_version
    OUTPUT_STRIP_TRAILING_WHITESPACE)
  string(REPLACE "\n" ", " xcode_version ${xcode_version})
  message(STATUS "${xcode_version}")
  message(STATUS "")

  is_sdk_requested(OSX swift_build_osx)
  if(swift_build_osx)
    configure_sdk_darwin(
        OSX "OS X" "${SWIFT_DARWIN_DEPLOYMENT_VERSION_OSX}"
        macosx macosx macosx "x86_64")
    configure_target_variant(OSX-DA "OS X Debug+Asserts"   OSX DA "Debug+Asserts")
    configure_target_variant(OSX-RA "OS X Release+Asserts" OSX RA "Release+Asserts")
    configure_target_variant(OSX-R  "OS X Release"         OSX R  "Release")
  endif()

  if(XCODE)
    # FIXME: Cannot cross-compile stdlib using Xcode.  Xcode insists on
    # passing -mmacosx-version-min to the compiler, and we want to pass
    # -mios-version-min.  Clang sees both options and complains.
    set(swift_can_crosscompile_stdlib FALSE)
  else()
    set(swift_can_crosscompile_stdlib TRUE)
  endif()

  is_sdk_requested(IOS swift_build_ios)
  if(swift_build_ios AND ${swift_can_crosscompile_stdlib})
    configure_sdk_darwin(
        IOS "iOS" "${SWIFT_DARWIN_DEPLOYMENT_VERSION_IOS}"
        iphoneos ios ios "armv7;armv7s;arm64")
    configure_target_variant(IOS-DA "iOS Debug+Asserts"   IOS DA "Debug+Asserts")
    configure_target_variant(IOS-RA "iOS Release+Asserts" IOS RA "Release+Asserts")
    configure_target_variant(IOS-R  "iOS Release"         IOS R "Release")
  endif()

  is_sdk_requested(IOS_SIMULATOR swift_build_ios_simulator)
  if(swift_build_ios_simulator AND ${swift_can_crosscompile_stdlib})
    configure_sdk_darwin(
        IOS_SIMULATOR "iOS Simulator" "${SWIFT_DARWIN_DEPLOYMENT_VERSION_IOS}"
        iphonesimulator ios-simulator ios "i386;x86_64")
    configure_target_variant(
        IOS_SIMULATOR-DA "iOS Debug+Asserts"   IOS_SIMULATOR DA "Debug+Asserts")
    configure_target_variant(
        IOS_SIMULATOR-RA "iOS Release+Asserts" IOS_SIMULATOR RA "Release+Asserts")
    configure_target_variant(
        IOS_SIMULATOR-R  "iOS Release"         IOS_SIMULATOR R "Release")
  endif()

  is_sdk_requested(TVOS swift_build_tvos)
  if(swift_build_tvos AND ${swift_can_crosscompile_stdlib})
    configure_sdk_darwin(
        TVOS "tvOS" "${SWIFT_DARWIN_DEPLOYMENT_VERSION_TVOS}"
        appletvos tvos tvos "arm64")
    configure_target_variant(TVOS-DA "tvOS Debug+Asserts"   TVOS DA "Debug+Asserts")
    configure_target_variant(TVOS-RA "tvOS Release+Asserts" TVOS RA "Release+Asserts")
    configure_target_variant(TVOS-R  "tvOS Release"         TVOS R "Release")
  endif()

  is_sdk_requested(TVOS_SIMULATOR swift_build_tvos_simulator)
  if(swift_build_tvos_simulator AND ${swift_can_crosscompile_stdlib})
    configure_sdk_darwin(
        TVOS_SIMULATOR "tvOS Simulator" "${SWIFT_DARWIN_DEPLOYMENT_VERSION_TVOS}"
        appletvsimulator tvos-simulator tvos "x86_64")
    configure_target_variant(
      TVOS_SIMULATOR-DA "tvOS Debug+Asserts"   TVOS_SIMULATOR DA "Debug+Asserts")
    configure_target_variant(
      TVOS_SIMULATOR-RA "tvOS Release+Asserts" TVOS_SIMULATOR RA "Release+Asserts")
    configure_target_variant(
      TVOS_SIMULATOR-R  "tvOS Release"         TVOS_SIMULATOR R "Release")
  endif()

  is_sdk_requested(WATCHOS swift_build_watchos)
  if(swift_build_watchos AND ${swift_can_crosscompile_stdlib})
    configure_sdk_darwin(
        WATCHOS "watchOS" "${SWIFT_DARWIN_DEPLOYMENT_VERSION_WATCHOS}"
        watchos watchos watchos "armv7k")
    configure_target_variant(WATCHOS-DA "watchOS Debug+Asserts"   WATCHOS DA "Debug+Asserts")
    configure_target_variant(WATCHOS-RA "watchOS Release+Asserts" WATCHOS RA "Release+Asserts")
    configure_target_variant(WATCHOS-R  "watchOS Release"         WATCHOS R "Release")
  endif()

  is_sdk_requested(WATCHOS_SIMULATOR swift_build_watchos_simulator)
  if(swift_build_watchos_simulator AND ${swift_can_crosscompile_stdlib})
    configure_sdk_darwin(
        WATCHOS_SIMULATOR "watchOS Simulator" "${SWIFT_DARWIN_DEPLOYMENT_VERSION_WATCHOS}"
        watchsimulator watchos-simulator watchos "i386")
    configure_target_variant(WATCHOS_SIMULATOR-DA "watchOS Debug+Asserts"   WATCHOS_SIMULATOR DA "Debug+Asserts")
    configure_target_variant(WATCHOS_SIMULATOR-RA "watchOS Release+Asserts" WATCHOS_SIMULATOR RA "Release+Asserts")
    configure_target_variant(WATCHOS_SIMULATOR-R  "watchOS Release"         WATCHOS_SIMULATOR R "Release")
  endif()

  # FIXME: guess target variant based on the host.
  # if(SWIFT_HOST_VARIANT MATCHES "^macosx")
  #   set(SWIFT_PRIMARY_VARIANT_GUESS "OSX-R")
  # elseif(SWIFT_HOST_VARIANT MATCHES "^iphoneos")
  #   set(SWIFT_PRIMARY_VARIANT_GUESS "IOS-R")
  # else()
  #   message(FATAL_ERROR "Unknown SWIFT_HOST_VARIANT '${SWIFT_HOST_VARIANT}'")
  # endif()
  #
  # set(SWIFT_PRIMARY_VARIANT ${SWIFT_PRIMARY_VARIANT_GUESS} CACHE STRING
  #    "[OSX-DA, OSX-RA, OSX-R, IOS-DA, IOS-RA, IOS-R, IOS_SIMULATOR-DA, IOS_SIMULATOR-RA, IOS_SIMULATOR-R]")
  #
  # FIXME: hardcode OS X as the default variant for now.
  set(SWIFT_PRIMARY_VARIANT_SDK_default "OSX")
  set(SWIFT_PRIMARY_VARIANT_ARCH_default "x86_64")
endif()

if("${SWIFT_PRIMARY_VARIANT_SDK}" STREQUAL "")
  set(SWIFT_PRIMARY_VARIANT_SDK "${SWIFT_PRIMARY_VARIANT_SDK_default}")
endif()
if("${SWIFT_PRIMARY_VARIANT_ARCH}" STREQUAL "")
  set(SWIFT_PRIMARY_VARIANT_ARCH "${SWIFT_PRIMARY_VARIANT_ARCH_default}")
endif()

if("${SWIFT_SDKS}" STREQUAL "")
  set(SWIFT_SDKS "${SWIFT_CONFIGURED_SDKS}")
endif()

list_subtract("${SWIFT_SDKS}" "${SWIFT_CONFIGURED_SDKS}" unknown_sdks)

if(NOT "${unknown_sdks}" STREQUAL "")
  message(FATAL_ERROR "Unknown SDKs: ${unknown_sdks}")
endif()

if("${SWIFT_CONFIGURED_SDKS}" STREQUAL "")
  message(FATAL_ERROR "No SDKs selected.")
endif()

if("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "")
  message(FATAL_ERROR "No SDK for host tools.")
endif()

if("${SWIFT_HOST_VARIANT_ARCH}" STREQUAL "")
  message(FATAL_ERROR "No arch for host tools.")
endif()

set(SWIFT_PRIMARY_VARIANT_SUFFIX
    "-${SWIFT_SDK_${SWIFT_PRIMARY_VARIANT_SDK}_LIB_SUBDIR}-${SWIFT_PRIMARY_VARIANT_ARCH}")

# Clear universal library names to prevent adding duplicates
foreach(sdk ${SWIFT_SDKS})
  unset(UNIVERSAL_LIBRARY_NAMES_${SWIFT_SDK_${sdk}_LIB_SUBDIR} CACHE)
endforeach()

if(SWIFT_PARALLEL_LINK_JOBS)
  if(CMAKE_VERSION VERSION_LESS 3.0 OR NOT CMAKE_MAKE_PROGRAM MATCHES "ninja")
    message(WARNING "Job pooling is only available with Ninja generators and CMake 3.0 and later.")
  else()
    set_property(GLOBAL APPEND PROPERTY JOB_POOLS swift_link_job_pool=${SWIFT_PARALLEL_LINK_JOBS})
    set(CMAKE_JOB_POOL_LINK swift_link_job_pool)
  endif()
endif()

message(STATUS "Building host Swift tools for ${SWIFT_HOST_VARIANT_SDK} ${SWIFT_HOST_VARIANT_ARCH}")
message(STATUS "  Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  Assertions: ${LLVM_ENABLE_ASSERTIONS}")
message(STATUS "  LTO:        ${SWIFT_ENABLE_LTO}")
message(STATUS "")

message(STATUS "Building Swift standard library and SDK overlays for SDKs: ${SWIFT_SDKS}")
message(STATUS "  Build type: ${SWIFT_STDLIB_BUILD_TYPE}")
message(STATUS "  Assertions: ${SWIFT_STDLIB_ASSERTIONS}")
message(STATUS "")

message(STATUS "Building Swift runtime with:")
message(STATUS "  Leak Detection Checker Entrypoints: ${SWIFT_RUNTIME_ENABLE_LEAK_CHECKER}")
message(STATUS "")

#
# Find optional dependencies.
#

# Find libxml.
# FIXME: unify with CLANG_HAVE_LIBXML, which is set in LLVM anyway.
find_package(LibXml2)
if(LIBXML2_FOUND)
  set(SWIFT_HAVE_LIBXML 1)
endif()

if (LLVM_ENABLE_DOXYGEN)
  message(STATUS "Doxygen: enabled")
endif()

#
# Set up global CMake variables for API notes.
#
# API notes version 1.0.0
#
# Change the above comment to 'touch' this file and keep incremental builds
# working when adding a new apinotes file.
#
set(SWIFT_API_NOTES_PATH "${SWIFT_SOURCE_DIR}/apinotes")
if(NOT EXISTS "${SWIFT_API_NOTES_PATH}/Foundation.apinotes")
  message(FATAL_ERROR "API notes are not available in ${SWIFT_API_NOTES_PATH}")
endif()

file(GLOB SWIFT_API_NOTES_INPUT_FILES "${SWIFT_API_NOTES_PATH}/*.apinotes")
foreach(file ${SWIFT_API_NOTES_INPUT_FILES})
  get_filename_component(name "${file}" NAME_WE)
  list(APPEND SWIFT_API_NOTES_INPUTS ${name})
endforeach()

# Add all of the subdirectories, where we actually do work.
if(SWIFT_BUILD_TOOLS)
  add_subdirectory(include)
  add_subdirectory(lib)
  add_subdirectory(tools)
  add_subdirectory(tools/swift-reflection-dump)
endif()
add_subdirectory(utils)
add_subdirectory(stdlib)

if(SWIFT_BUILD_STDLIB AND SWIFT_INCLUDE_TESTS)
  add_subdirectory(tools/swift-reflection-test)
endif()

if(SWIFT_BUILD_PERF_TESTSUITE AND "${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  add_subdirectory(benchmark)
endif()
if(SWIFT_INCLUDE_TESTS)
  add_subdirectory(test)
  add_subdirectory(unittests)
endif()
if(SWIFT_INCLUDE_DOCS)
  add_subdirectory(docs)
endif()

swift_install_in_component(license
  FILES "LICENSE.txt"
  DESTINATION "share/swift")

# Add a documentation target so that documentation shows up in the
# Xcode project.
if(XCODE)
  add_custom_target(Documentation
      SOURCES
        README.md
        docs)

  file(GLOB SWIFT_TOPLEVEL_HEADERS
      ${CMAKE_CURRENT_SOURCE_DIR}/include/swift${dir}/*.h
      ${CMAKE_CURRENT_SOURCE_DIR}/include/swift${dir}/*.td
      ${CMAKE_CURRENT_SOURCE_DIR}/include/swift${dir}/*.def)
  add_custom_target(Miscellaneous
      SOURCES ${SWIFT_TOPLEVEL_HEADERS})
endif()
