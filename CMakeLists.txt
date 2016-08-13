cmake_minimum_required(VERSION 2.8.12)

if(POLICY CMP0051)
  cmake_policy(SET CMP0051 NEW)
endif()

if(POLICY CMP0054)
  cmake_policy(SET CMP0054 NEW)
endif()

# Add path for custom CMake modules.
list(APPEND CMAKE_MODULE_PATH
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

# First include general CMake utilities.
include(SwiftUtils)

#
# User-configurable options that control the inclusion and default build
# behavior for components which may not strictly be necessary (tools, examples,
# and tests).
#
# This is primarily to support building smaller or faster project files.
#

option(SWIFT_INCLUDE_TOOLS
    "Generate build targets for swift tools"
    TRUE)

option(SWIFT_BUILD_REMOTE_MIRROR
    "Build the Swift Remote Mirror Library"
    TRUE)

option(SWIFT_BUILD_DYNAMIC_STDLIB
    "Build dynamic variants of the Swift standard library"
    TRUE)

option(SWIFT_BUILD_STATIC_STDLIB
    "Build static variants of the Swift standard library"
    FALSE)

option(SWIFT_BUILD_DYNAMIC_SDK_OVERLAY
    "Build dynamic variants of the Swift SDK overlay"
    TRUE)

option(SWIFT_BUILD_STATIC_SDK_OVERLAY
    "Build static variants of the Swift SDK overlay"
    FALSE)

# In many cases, the CMake build system needs to determine whether to include
# a directory, or perform other actions, based on whether the stdlib or SDK is
# being built at all -- statically or dynamically. Please note that these
# flags are not related to the deprecated build-script-impl arguments
# 'build-swift-stdlib' and 'build-swift-sdk-overlay'. These are not flags that
# the build script should be able to set.
if(SWIFT_BUILD_DYNAMIC_STDLIB OR SWIFT_BUILD_STATIC_STDLIB)
  set(SWIFT_BUILD_STDLIB TRUE)
else()
  set(SWIFT_BUILD_STDLIB FALSE)
endif()

if(SWIFT_BUILD_DYNAMIC_SDK_OVERLAY OR SWIFT_BUILD_STATIC_SDK_OVERLAY)
  set(SWIFT_BUILD_SDK_OVERLAY TRUE)
else()
  set(SWIFT_BUILD_SDK_OVERLAY FALSE)
endif()

option(SWIFT_BUILD_PERF_TESTSUITE
    "Create targets for swift performance benchmarks."
    FALSE)

option(SWIFT_INCLUDE_TESTS "Create targets for building/running tests." TRUE)

option(SWIFT_INCLUDE_DOCS
    "Create targets for building docs."
    TRUE)

#
# Miscellaneous User-configurable options.
#
# TODO: Please categorize these!
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

option(SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER
       "Use the host compiler and not the internal clang to build the swift runtime"
       FALSE)
option(SWIFT_BUILD_RUNTIME_WITH_HOST_LIBLTO
       "Use the host liblto and not the internal liblto to build the swift runtime"
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

# Indicate whether Swift should attempt to use the lld linker.
set(SWIFT_ENABLE_LLD_LINKER FALSE CACHE BOOL
    "Enable using the lld linker when available")

# Indicate whether Swift should attempt to use the gold linker.
# This is not used on Darwin.
set(SWIFT_ENABLE_GOLD_LINKER FALSE CACHE BOOL
    "Enable using the gold linker when available")

set(_SWIFT_KNOWN_INSTALL_COMPONENTS
    "autolink-driver;compiler;clang-builtin-headers;clang-resource-dir-symlink;clang-builtin-headers-in-clang-resource-dir;stdlib;stdlib-experimental;sdk-overlay;editor-integration;tools;testsuite-tools;toolchain-dev-tools;dev;license;sourcekit-xpc-service;sourcekit-inproc;swift-remote-mirror;swift-remote-mirror-headers")

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
# * toolchain-dev-tools -- install development tools useful in a shared toolchain
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

set(SWIFT_TOOLS_ENABLE_LTO OFF CACHE STRING "Build Swift tools with LTO. One
    must specify the form of LTO by setting this to one of: 'full', 'thin'. This
    option only affects the tools that run on the host (the compiler), and has
    no effect on the target libraries (the standard library and the runtime).")

# The following only works with the Ninja generator in CMake >= 3.0.
set(SWIFT_PARALLEL_LINK_JOBS "" CACHE STRING
  "Define the maximum number of linker jobs for swift.")

#
# User-configurable Android specific options.
#

set(SWIFT_ANDROID_NDK_PATH "" CACHE STRING
  "Path to the directory that contains the Android NDK tools that are executable on the build machine")
set(SWIFT_ANDROID_NDK_GCC_VERSION "" CACHE STRING
  "The GCC version to use when building for Android. Currently only 4.9 is supported.")
set(SWIFT_ANDROID_SDK_PATH "" CACHE STRING
  "Path to the directory that contains the Android SDK tools that will be passed to the swiftc frontend")
set(SWIFT_ANDROID_DEPLOY_DEVICE_PATH "" CACHE STRING
  "Path on an Android device where build products will be pushed. These are used when running the test suite against the device")

#
# User-configurable ICU specific options for Android, FreeBSD, Linux.
#

foreach(sdk ANDROID;FREEBSD;LINUX;WINDOWS)
  set(SWIFT_${sdk}_ICU_UC "" CACHE STRING
      "Path to a directory containing the icuuc library for ${sdk}")
  set(SWIFT_${sdk}_ICU_UC_INCLUDE "" CACHE STRING
      "Path to a directory containing headers for icuuc for ${sdk}")
  set(SWIFT_${sdk}_ICU_I18N "" CACHE STRING
      "Path to a directory containing the icui18n library for ${sdk}")
  set(SWIFT_${sdk}_ICU_I18N_INCLUDE "" CACHE STRING
      "Path to a directory containing headers icui18n for ${sdk}")
endforeach()

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

option(SWIFT_SERIALIZE_STDLIB_UNITTEST
    "Compile the StdlibUnittest module with -sil-serialize-all to increase the test coverage for the optimizer"
    FALSE)

option(SWIFT_STDLIB_SIL_DEBUGGING
    "Compile the Swift standard library with -gsil to enable debugging and profiling on SIL level"
    FALSE)

option(SWIFT_CHECK_INCREMENTAL_COMPILATION
    "Check if incremental compilation works when compiling the Swift libraries"
    FALSE)

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
# Assume a new enough ar to generate the index at construction time. This avoids
# having to invoke ranlib as a secondary command.
#

set(CMAKE_C_ARCHIVE_CREATE "<CMAKE_AR> crs <TARGET> <LINK_FLAGS> <OBJECTS>")
set(CMAKE_C_ARCHIVE_APPEND "<CMAKE_AR> qs <TARGET> <LINK_FLAGS> <OBJECTS>")
set(CMAKE_C_ARCHIVE_FINISH "")

set(CMAKE_CXX_ARCHIVE_CREATE "<CMAKE_AR> crs <TARGET> <LINK_FLAGS> <OBJECTS>")
set(CMAKE_CXX_ARCHIVE_APPEND "<CMAKE_AR> qs <TARGET> <LINK_FLAGS> <OBJECTS>")
set(CMAKE_CXX_ARCHIVE_FINISH "")

#
# Include CMake modules
#

include(CheckCXXSourceRuns)
include(CMakeParseArguments)
include(SwiftHandleGybSources)
include(SwiftSetIfArchBitness)
include(SwiftSource)
include(AddSwift)
include(SwiftConfigureSDK)
include(SwiftComponents)
include(SwiftList)

swift_configure_install_components("${SWIFT_INSTALL_COMPONENTS}")

if("${CMAKE_VERSION}" VERSION_LESS "3.0")
  set(SWIFT_CMAKE_HAS_GENERATOR_EXPRESSIONS FALSE)
else()
  set(SWIFT_CMAKE_HAS_GENERATOR_EXPRESSIONS TRUE)
endif()

# lipo is used to create universal binaries.
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  include(SwiftDarwin)
  find_toolchain_tool(LIPO "${SWIFT_DARWIN_XCRUN_TOOLCHAIN}" lipo)
endif()

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

set(SWIFT_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
set(SWIFT_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}")

set(SWIFT_RUNTIME_OUTPUT_INTDIR "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bin")
set(SWIFT_LIBRARY_OUTPUT_INTDIR "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib")
if("${SWIFT_NATIVE_SWIFT_TOOLS_PATH}" STREQUAL "")
  set(SWIFT_NATIVE_SWIFT_TOOLS_PATH "${SWIFT_RUNTIME_OUTPUT_INTDIR}")
endif()

# This setting causes all CMakeLists.txt to automatically have
# ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CURRENT_SOURCE_DIR} as an
# include_directories path. This is done for developer
# convenience. Additionally, LLVM/Clang build with this option enabled, so we
# should match them unless it is removed from LLVM/Clang as well.
#
# *NOTE* Even though these directories are added to the include path for a
# specific CMakeLists.txt, these include paths are not propagated down to
# subdirectories.
set(CMAKE_INCLUDE_CURRENT_DIR ON)

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

include(SwiftCheckCXXNativeRegex)
check_cxx_native_regex(SWIFT_HAVE_WORKING_STD_REGEX)

# If SWIFT_HOST_VARIANT_SDK not given, try to detect from the CMAKE_SYSTEM_NAME.
if(SWIFT_HOST_VARIANT_SDK)
  set(SWIFT_HOST_VARIANT_SDK_default "${SWIFT_HOST_VARIANT_SDK}")
else()
  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    set(SWIFT_HOST_VARIANT_SDK_default "LINUX")
  elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "FreeBSD")
    set(SWIFT_HOST_VARIANT_SDK_default "FREEBSD")
  elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "CYGWIN")
    set(SWIFT_HOST_VARIANT_SDK_default "CYGWIN")
  elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    set(SWIFT_HOST_VARIANT_SDK_default "OSX")
  else()
    message(FATAL_ERROR "Unable to detect SDK for host system: ${CMAKE_SYSTEM_NAME}")
  endif()
endif()

# If SWIFT_HOST_VARIANT_ARCH not given, try to detect from the CMAKE_SYSTEM_PROCESSOR.
if(SWIFT_HOST_VARIANT_ARCH)
  set(SWIFT_HOST_VARIANT_ARCH_default, "${SWIFT_HOST_VARIANT_ARCH}")
else()
  if("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
    set(SWIFT_HOST_VARIANT_ARCH_default "x86_64")
  elseif("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "aarch64")
    set(SWIFT_HOST_VARIANT_ARCH_default "aarch64")
  elseif("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "ppc64")
    set(SWIFT_HOST_VARIANT_ARCH_default "powerpc64")
  elseif("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "ppc64le")
    set(SWIFT_HOST_VARIANT_ARCH_default "powerpc64le")
  elseif("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "s390x")
    set(SWIFT_HOST_VARIANT_ARCH_default "s390x")
  # FIXME: Only matches v6l/v7l - by far the most common variants
  elseif("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "armv6l")
    set(SWIFT_HOST_VARIANT_ARCH_default "armv6")
  elseif("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "armv7l")
    set(SWIFT_HOST_VARIANT_ARCH_default "armv7")
  else()
    message(FATAL_ERROR "Unrecognized architecture on host system: ${CMAKE_SYSTEM_PROCESSOR}")
  endif()
endif()

set(SWIFT_HOST_VARIANT_SDK "${SWIFT_HOST_VARIANT_SDK_default}" CACHE STRING
    "Deployment sdk for Swift host tools (the compiler).")
set(SWIFT_HOST_VARIANT_ARCH "${SWIFT_HOST_VARIANT_ARCH_default}" CACHE STRING
    "Deployment arch for Swift host tools (the compiler).")

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
    list(FIND SWIFT_SDKS "${name}" sdk_index)
    if(${sdk_index} EQUAL -1)
      set("${result_var_name}" "FALSE" PARENT_SCOPE)
    else()
      set("${result_var_name}" "TRUE" PARENT_SCOPE)
    endif()
  endif()
endfunction()

if(XCODE)
  # FIXME: Cannot cross-compile the standard library using Xcode.  Xcode
  # insists on passing -mmacosx-version-min to the compiler, and we need
  # to pass -mios-version-min.  Clang sees both options and complains.
  set(SWIFT_SDKS "OSX")
endif()

# FIXME: the parameters we specify in SWIFT_SDKS are lacking architecture specifics,
# so we need to hard-code it. For example, the SDK for Android is just 'ANDROID', 
# which we assume below to be armv7. 
# The iOS SDKs all have their architectures hardcoded because they are just specified by name (e.g. 'IOS' or 'WATCHOS').
# We can't cross-compile the standard library for another linux architecture,
# because the SDK list would just be 'LINUX' and we couldn't disambiguate it from the host.
# 
# To fix it, we would need to append the architecture to the SDKs,
# for example: 'OSX-x86_64;IOS-armv7;...etc'.
# We could easily do that - we have all of that information in build-script-impl.
# Also, we would need to be provided with the sysroot for each SDK (see SWIFT_ANDROID_SDK_PATH/SWIFT_SDK_ANDROID_PATH).
# Darwin targets cheat and use `xcrun`.

if("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "LINUX")
  
  set(CMAKE_EXECUTABLE_FORMAT "ELF")
  set(SWIFT_HOST_VARIANT "linux" CACHE STRING
      "Deployment OS for Swift host tools (the compiler) [linux].")

  # Calculate the host triple
  if("${SWIFT_HOST_TRIPLE}" STREQUAL "")
    if("${SWIFT_HOST_VARIANT_ARCH}" STREQUAL "x86_64")
      set(SWIFT_HOST_TRIPLE "x86_64-unknown-linux-gnu")
    elseif("${SWIFT_HOST_VARIANT_ARCH}" STREQUAL "aarch64")
      set(SWIFT_HOST_TRIPLE "aarch64-unknown-linux-gnu")
    elseif("${SWIFT_HOST_VARIANT_ARCH}" MATCHES "(powerpc64|powerpc64le)")
      set(SWIFT_HOST_TRIPLE "${SWIFT_HOST_VARIANT_ARCH}-unknown-linux-gnu")
    elseif("${SWIFT_HOST_VARIANT_ARCH}" MATCHES "s390x")
      set(SWIFT_HOST_TRIPLE "s390x-unknown-linux-gnu")
    elseif("${SWIFT_HOST_VARIANT_ARCH}" MATCHES "(armv6|armv7)")
      set(SWIFT_HOST_TRIPLE "${SWIFT_HOST_VARIANT_ARCH}-unknown-linux-gnueabihf")
    else()
      message(FATAL_ERROR "Unable to calculate triple for linux host on ${SWIFT_HOST_VARIANT_ARCH}")
    endif()
  endif()

  # Should we build the standard library for the host?
  is_sdk_requested(LINUX swift_build_linux)
  if(swift_build_linux)
    configure_sdk_unix(LINUX "Linux" "linux" "${SWIFT_HOST_VARIANT}" "${SWIFT_HOST_VARIANT_ARCH}" "${SWIFT_HOST_TRIPLE}" "/")
    set(SWIFT_PRIMARY_VARIANT_SDK_default  "${SWIFT_HOST_VARIANT_SDK}")
    set(SWIFT_PRIMARY_VARIANT_ARCH_default "${SWIFT_HOST_VARIANT_ARCH}")
  endif()

  # Compatible cross-compile SDKS for LINUX: ANDROID (arch always armv7)
  is_sdk_requested(ANDROID swift_build_android)
  if("${SWIFT_ANDROID_NDK_PATH}" STREQUAL "")
    set(swift_can_crosscompile_stdlib_android FALSE)
  else()
    set(swift_can_crosscompile_stdlib_android TRUE)
  endif()

  if(swift_build_android AND ${swift_can_crosscompile_stdlib_android})
    configure_sdk_unix(ANDROID "Android" "android" "android" "armv7" "armv7-none-linux-androideabi" "${SWIFT_ANDROID_SDK_PATH}")
    # If we're not building for the host, the cross-compiled target should be the 'primary variant'.
    if("${swift_build_linux}" STREQUAL "FALSE")
      set(SWIFT_PRIMARY_VARIANT_SDK_default "ANDROID")
      set(SWIFT_PRIMARY_VARIANT_ARCH_default "armv7")
    endif()
  endif()

elseif("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "FREEBSD")
  
  set(CMAKE_EXECUTABLE_FORMAT "ELF")
  set(SWIFT_HOST_VARIANT "freebsd" CACHE STRING
      "Deployment OS for Swift host tools (the compiler) [freebsd].")

  # FIXME: Using the host OS version won't produce correct results for
  # cross-compilation.
  string(REPLACE "[-].*" "" FREEBSD_SYSTEM_VERSION ${CMAKE_SYSTEM_VERSION})
  message(STATUS "FreeBSD Version: ${FREEBSD_SYSTEM_VERSION}")
  configure_sdk_unix(FREEBSD "FreeBSD" "freebsd" "freebsd" "x86_64"
    "x86_64-unknown-freebsd${FREEBSD_SYSTEM_VERSION}" "/")
  set(SWIFT_PRIMARY_VARIANT_SDK_default  "${SWIFT_HOST_VARIANT_SDK}")
  set(SWIFT_PRIMARY_VARIANT_ARCH_default "x86_64")
  
elseif("${SWIFT_HOST_VARIANT_SDK}" STREQUAL "CYGWIN")
  
  # set(CMAKE_EXECUTABLE_FORMAT "ELF")
  set(SWIFT_HOST_VARIANT "windows" CACHE STRING
      "Deployment OS for Swift host tools (the compiler) [windows].")
  
  configure_sdk_unix(CYGWIN "Cygwin" "windows" "cygwin" "windows" "x86_64-unknown-windows-cygnus" "/")
  set(SWIFT_PRIMARY_VARIANT_SDK_default  "${SWIFT_HOST_VARIANT_SDK}")
  set(SWIFT_PRIMARY_VARIANT_ARCH_default "x86_64")

elseif("${SWIFT_HOST_VARIANT_SDK}" MATCHES "(OSX|IOS*|TVOS*|WATCHOS*)")

  set(SWIFT_HOST_VARIANT "macosx" CACHE STRING
      "Deployment OS for Swift host tools (the compiler) [macosx, iphoneos].")

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

  # Compatible cross-compile SDKS for Darwin OSes: IOS, IOS_SIMULATOR, TVOS,
  #   TVOS_SIMULATOR, WATCHOS, WATCHOS_SIMULATOR (archs hardcoded below).

  is_sdk_requested(IOS swift_build_ios)
  if(swift_build_ios)
    configure_sdk_darwin(
        IOS "iOS" "${SWIFT_DARWIN_DEPLOYMENT_VERSION_IOS}"
        iphoneos ios ios "armv7;armv7s;arm64")
    configure_target_variant(IOS-DA "iOS Debug+Asserts"   IOS DA "Debug+Asserts")
    configure_target_variant(IOS-RA "iOS Release+Asserts" IOS RA "Release+Asserts")
    configure_target_variant(IOS-R  "iOS Release"         IOS R "Release")
  endif()

  is_sdk_requested(IOS_SIMULATOR swift_build_ios_simulator)
  if(swift_build_ios_simulator)
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
  if(swift_build_tvos)
    configure_sdk_darwin(
        TVOS "tvOS" "${SWIFT_DARWIN_DEPLOYMENT_VERSION_TVOS}"
        appletvos tvos tvos "arm64")
    configure_target_variant(TVOS-DA "tvOS Debug+Asserts"   TVOS DA "Debug+Asserts")
    configure_target_variant(TVOS-RA "tvOS Release+Asserts" TVOS RA "Release+Asserts")
    configure_target_variant(TVOS-R  "tvOS Release"         TVOS R "Release")
  endif()

  is_sdk_requested(TVOS_SIMULATOR swift_build_tvos_simulator)
  if(swift_build_tvos_simulator)
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
  if(swift_build_watchos)
    configure_sdk_darwin(
        WATCHOS "watchOS" "${SWIFT_DARWIN_DEPLOYMENT_VERSION_WATCHOS}"
        watchos watchos watchos "armv7k")
    configure_target_variant(WATCHOS-DA "watchOS Debug+Asserts"   WATCHOS DA "Debug+Asserts")
    configure_target_variant(WATCHOS-RA "watchOS Release+Asserts" WATCHOS RA "Release+Asserts")
    configure_target_variant(WATCHOS-R  "watchOS Release"         WATCHOS R "Release")
  endif()

  is_sdk_requested(WATCHOS_SIMULATOR swift_build_watchos_simulator)
  if(swift_build_watchos_simulator)
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
  # Primary variant is always OSX; even on iOS hosts.
  set(SWIFT_PRIMARY_VARIANT_SDK_default "OSX")
  set(SWIFT_PRIMARY_VARIANT_ARCH_default "x86_64")

endif()

if("${SWIFT_PRIMARY_VARIANT_SDK}" STREQUAL "")
  set(SWIFT_PRIMARY_VARIANT_SDK "${SWIFT_PRIMARY_VARIANT_SDK_default}")
endif()
if("${SWIFT_PRIMARY_VARIANT_ARCH}" STREQUAL "")
  set(SWIFT_PRIMARY_VARIANT_ARCH "${SWIFT_PRIMARY_VARIANT_ARCH_default}")
endif()

# Should we cross-compile the standard library for Windows?
is_sdk_requested(WINDOWS swift_build_windows)
if(swift_build_windows)
  configure_sdk_windows(WINDOWS "Windows" "msvc" i686)
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

# Set the CMAKE_OSX_* variables in a way that minimizes conflicts.
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  set(CMAKE_OSX_SYSROOT "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_PATH}")
  set(CMAKE_OSX_ARCHITECTURES "")
  set(CMAKE_OSX_DEPLOYMENT_TARGET "")
endif()

message(STATUS "Building host Swift tools for ${SWIFT_HOST_VARIANT_SDK} ${SWIFT_HOST_VARIANT_ARCH}")
message(STATUS "  Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  Assertions: ${LLVM_ENABLE_ASSERTIONS}")
message(STATUS "  LTO:        ${SWIFT_TOOLS_ENABLE_LTO}")
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
set(SWIFT_API_NOTES_PATH "${SWIFT_SOURCE_DIR}/apinotes")
include("${SWIFT_API_NOTES_PATH}/CMakeLists.txt")
if(NOT DEFINED SWIFT_API_NOTES_INPUTS)
  message(FATAL_ERROR "API notes are not available in ${SWIFT_API_NOTES_PATH}")
endif()

# Add all of the subdirectories, where we actually do work.
if(SWIFT_INCLUDE_TOOLS)
  add_subdirectory(include)
  add_subdirectory(lib)
  add_subdirectory(tools)
endif()

add_subdirectory(utils)
add_subdirectory(stdlib)

if(SWIFT_BUILD_DYNAMIC_STDLIB AND SWIFT_INCLUDE_TESTS)
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
