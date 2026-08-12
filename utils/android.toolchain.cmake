# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

# Toolchain file for Android targets built with the Swift toolchain's own clang
# rather than the one shipped in the NDK.

# TODO(Steelskin): Every change that we do here should be handled either in the
# NDK or in CMake, so that we can eventually remove this file and simply pass a
# specific compiler path and the NDK location to CMake.
#
# Inputs, passed with `-D`:
#   CMAKE_ANDROID_NDK       path to the NDK
#   CMAKE_ANDROID_API       API level, e.g. 23
#   CMAKE_ANDROID_ARCH_ABI  ABI name, e.g. arm64-v8a
#
# Everything else is derived here, matching what a "regular" CMake + NDK build
# does. This is re-run for every `try_compile` CMake performs and sets the
# right flags for every compiler.
set(CMAKE_TRY_COMPILE_PLATFORM_VARIABLES
  CMAKE_ANDROID_NDK
  CMAKE_ANDROID_API
  CMAKE_ANDROID_ARCH_ABI)

set(CMAKE_SYSTEM_NAME Android)

if(NOT CMAKE_ANDROID_NDK)
  message(FATAL_ERROR
    "CMAKE_ANDROID_NDK is required by ${CMAKE_CURRENT_LIST_FILE}.")
endif()
if(NOT IS_DIRECTORY "${CMAKE_ANDROID_NDK}")
  message(FATAL_ERROR
    "CMAKE_ANDROID_NDK does not name a directory: '${CMAKE_ANDROID_NDK}'")
endif()
if(NOT CMAKE_ANDROID_API)
  message(FATAL_ERROR
    "CMAKE_ANDROID_API is required by ${CMAKE_CURRENT_LIST_FILE}.")
endif()

# Set the root for the Android's prebuilt clang toolchain.
string(TOLOWER ${CMAKE_HOST_SYSTEM_NAME} _swift_android_host)
set(_swift_android_prebuilt
  "${CMAKE_ANDROID_NDK}/toolchains/llvm/prebuilt/${_swift_android_host}-x86_64")

set(CMAKE_SYSROOT "${_swift_android_prebuilt}/sysroot")

# Discover the NDK's clang rather than pinning its version.
file(GLOB _swift_android_ndk_runtimes "${_swift_android_prebuilt}/lib/clang/*")
list(SORT _swift_android_ndk_runtimes)
if(NOT _swift_android_ndk_runtimes)
  message(FATAL_ERROR
    "no clang resource directory under '${_swift_android_prebuilt}/lib/clang'; "
    "is CMAKE_ANDROID_NDK pointing at an NDK?")
endif()
list(GET _swift_android_ndk_runtimes -1 _swift_android_ndk_runtime)

# Set the target triple based on CMAKE_ANDROID_ARCH_ABI and CMAKE_ANDROID_API,
# similar to what the NDK does. Our triple differs from the NDK's in that it
# uses `unknown` for the vendor.
if(CMAKE_ANDROID_ARCH_ABI STREQUAL "arm64-v8a")
  set(_swift_android_triple "aarch64-unknown-linux-android${CMAKE_ANDROID_API}")
  set(_swift_android_ndk_arch aarch64)
elseif(CMAKE_ANDROID_ARCH_ABI STREQUAL "armeabi-v7a")
  set(_swift_android_triple "armv7-unknown-linux-androideabi${CMAKE_ANDROID_API}")
  set(_swift_android_ndk_arch arm)
elseif(CMAKE_ANDROID_ARCH_ABI STREQUAL "x86")
  set(_swift_android_triple "i686-unknown-linux-android${CMAKE_ANDROID_API}")
  set(_swift_android_ndk_arch i386)
elseif(CMAKE_ANDROID_ARCH_ABI STREQUAL "x86_64")
  set(_swift_android_triple "x86_64-unknown-linux-android${CMAKE_ANDROID_API}")
  set(_swift_android_ndk_arch x86_64)
else()
  message(FATAL_ERROR
    "unhandled CMAKE_ANDROID_ARCH_ABI '${CMAKE_ANDROID_ARCH_ABI}'")
endif()
set(CMAKE_C_COMPILER_TARGET ${_swift_android_triple})
set(CMAKE_Swift_COMPILER_TARGET ${_swift_android_triple})
set(CMAKE_CXX_COMPILER_TARGET ${_swift_android_triple})
set(CMAKE_ASM_COMPILER_TARGET ${_swift_android_triple})

# `libatomic` and `libunwind` live in the NDK's clang runtime directory rather
# than in the sysroot, so `--sysroot` alone does not find them.
#
# This has to live in `CMAKE_<LANG>_FLAGS` to survive
# `CMakeDetermineCompilerABI` and reach compiler-rt. The former blanks
# `CMAKE_<LANG>_STANDARD_LIBRARIES` and the latter sets link flags per target,
# preventing directory-level link options from reaching it.
# `-Qunused-arguments` comes along because a search path is unused when
# compiling, and it must be suppressed on the same command line.
#
# TODO(Steelskin): In CMake, populate `CMAKE_<LANG>_STANDARD_LINK_DIRECTORIES`
# in `Platform/Android-Common.cmake`.
# TODO(Steelskin): Upstream -Qunused-arguments in the NDK as
# `CMAKE_<LANG>_FLAGS_INIT`. Ideally, we'd be able to also add it to
# `CMAKE_Swift_FLAGS_INIT` as `-Xclang-linker -Qunused-arguments`.
set(_swift_android_flag_addition
  " \"-L${_swift_android_ndk_runtime}/lib/linux/${_swift_android_ndk_arch}\" \"-L${_swift_android_ndk_runtime}/lib/linux\" -Qunused-arguments")

foreach(_swift_android_lang C CXX ASM)
  # A literal search for exactly what is appended: idempotent across this file's
  # repeated reads and across the inheritance into every `try_compile`, without
  # matching a flag a user may have set for their own reasons.
  string(FIND "${CMAKE_${_swift_android_lang}_FLAGS}"
              "${_swift_android_flag_addition}" _swift_android_found)
  if(_swift_android_found EQUAL -1)
    set(CMAKE_${_swift_android_lang}_FLAGS
        "${CMAKE_${_swift_android_lang}_FLAGS}${_swift_android_flag_addition}")
  endif()
endforeach()

# CMake has no `CMAKE_SYSROOT` support for the Swift compiler, so pass it as a
# compile option.
# TODO(Steelskin): Upstream this in CMake behind a new policy and enable it in
# the Swift toolchain build.
add_compile_options("SHELL:$<$<COMPILE_LANGUAGE:Swift>:-sysroot \"${CMAKE_SYSROOT}\">")

# The NDK injects these through `CMAKE_*_LINKER_FLAGS_INIT` in `-Wl,` form,
# which the Swift driver cannot parse. build.ps1 passes `CMAKE_*_LINKER_FLAGS`
# as empty to stop the NDK flags from having an effect so we re-add them here.
# TODO(Steelskin): Modify the NDK to add linker flags as `LINKER:<flag>` rather
# than `-Wl,[...]` in `CMAKE_*_LINKER_FLAGS_INIT`, based on CMP0181.
add_link_options(
  LINKER:--build-id=sha1
  LINKER:--no-rosegment
  LINKER:--no-undefined-version
  LINKER:--fatal-warnings
  LINKER:--gc-sections
  LINKER:--no-undefined)

# Pass the target, sysroot and library search paths to the Swift driver as
# `-Xclang-linker` flags.
# TODO(Steelskin): Upstream this in CMake behind a new policy and enable it in
# the Swift toolchain build.
add_link_options(
  "SHELL:$<$<LINK_LANGUAGE:Swift>:-Xclang-linker -target -Xclang-linker ${_swift_android_triple}>"
  "SHELL:$<$<LINK_LANGUAGE:Swift>:-Xclang-linker --sysroot -Xclang-linker \"${CMAKE_SYSROOT}\">"
  "SHELL:$<$<LINK_LANGUAGE:Swift>:-Xclang-linker -Qunused-arguments>"
  "SHELL:$<$<LINK_LANGUAGE:Swift>:-L\"${_swift_android_ndk_runtime}/lib/linux/${_swift_android_ndk_arch}\" -L\"${_swift_android_ndk_runtime}/lib/linux\">")
