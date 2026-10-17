//===--- Swift.h - Swift standard library for C++ -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PUBLIC_SWIFT_H
#define SWIFT_PUBLIC_SWIFT_H

#if defined(__clang__)
#define SWIFT_STDLIB_TARGET_OS(os) __is_target_os(os)
#define SWIFT_STDLIB_TARGET_ENVIRONMENT(environment)                           \
  __is_target_environment(environment)
#define SWIFT_STDLIB_TARGET_VENDOR(vendor) __is_target_vendor(vendor)
#else
// C and Objective-C consumers can use a compiler without Clang target queries.
#define SWIFT_STDLIB_TARGET_OS(os) 0
#define SWIFT_STDLIB_TARGET_ENVIRONMENT(environment) 0
#define SWIFT_STDLIB_TARGET_VENDOR(vendor) 0
#endif

#if defined(__has_include)
#define SWIFT_STDLIB_HAS_INCLUDE(header) __has_include(header)
#else
// Older C preprocessors can include the target header without probing for it.
#define SWIFT_STDLIB_HAS_INCLUDE(header) 1
#endif

// Keep these directory names in sync with Swift's platform and architecture
// names. Compiler target queries avoid depending on an SDK's system headers.
// These paths are stringified; spaces around their separators change filenames.
// clang-format off
#if SWIFT_STDLIB_TARGET_ENVIRONMENT(macabi)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(maccatalyst/arch/_SwiftStdlib.h)
#elif SWIFT_STDLIB_TARGET_OS(macos) ||                                         \
    defined(__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(macosx/arch/_SwiftStdlib.h)
#elif SWIFT_STDLIB_TARGET_OS(ios)
#if SWIFT_STDLIB_TARGET_ENVIRONMENT(simulator)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(iphonesimulator/arch/_SwiftStdlib.h)
#else
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(iphoneos/arch/_SwiftStdlib.h)
#endif
#elif SWIFT_STDLIB_TARGET_OS(tvos)
#if SWIFT_STDLIB_TARGET_ENVIRONMENT(simulator)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(appletvsimulator/arch/_SwiftStdlib.h)
#else
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(appletvos/arch/_SwiftStdlib.h)
#endif
#elif SWIFT_STDLIB_TARGET_OS(watchos)
#if SWIFT_STDLIB_TARGET_ENVIRONMENT(simulator)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(watchsimulator/arch/_SwiftStdlib.h)
#else
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(watchos/arch/_SwiftStdlib.h)
#endif
#elif SWIFT_STDLIB_TARGET_OS(xros)
#if SWIFT_STDLIB_TARGET_ENVIRONMENT(simulator)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(xrsimulator/arch/_SwiftStdlib.h)
#else
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(xros/arch/_SwiftStdlib.h)
#endif
#elif defined(__ANDROID__)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(android/arch/_SwiftStdlib.h)
#elif defined(__linux__)
#if SWIFT_STDLIB_TARGET_ENVIRONMENT(musl) ||                                   \
    SWIFT_STDLIB_TARGET_ENVIRONMENT(musleabi) ||                               \
    SWIFT_STDLIB_TARGET_ENVIRONMENT(musleabihf)
#if SWIFT_STDLIB_TARGET_VENDOR(swift)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(linux-static/arch/_SwiftStdlib.h)
#else
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(musl/arch/_SwiftStdlib.h)
#endif
#else
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(linux/arch/_SwiftStdlib.h)
#endif
#elif defined(__CYGWIN__)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(cygwin/arch/_SwiftStdlib.h)
#elif defined(__MINGW32__)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(mingw/arch/_SwiftStdlib.h)
#elif defined(_WIN32)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(windows/arch/_SwiftStdlib.h)
#elif SWIFT_STDLIB_TARGET_OS(freebsd) || defined(__FreeBSD__)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(freebsd/arch/_SwiftStdlib.h)
#elif SWIFT_STDLIB_TARGET_OS(openbsd) || defined(__OpenBSD__)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(openbsd/arch/_SwiftStdlib.h)
#elif defined(__wasip2__)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(wasip2/arch/_SwiftStdlib.h)
#elif defined(__wasip3__)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(wasip3/arch/_SwiftStdlib.h)
#elif defined(__wasi__)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(wasi/arch/_SwiftStdlib.h)
#elif defined(__EMSCRIPTEN__)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(emscripten/arch/_SwiftStdlib.h)
#elif SWIFT_STDLIB_TARGET_OS(haiku)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(haiku/arch/_SwiftStdlib.h)
#elif SWIFT_STDLIB_TARGET_OS(ps4)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(ps4/arch/_SwiftStdlib.h)
#elif SWIFT_STDLIB_TARGET_OS(bridgeos)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(freestanding/arch/_SwiftStdlib.h)
#elif SWIFT_STDLIB_TARGET_OS(firmware)
#define SWIFT_STDLIB_HEADER_PLATFORM(arch)                                     \
  SWIFT_STDLIB_HEADER_PATH_IMPL(firmware/arch/_SwiftStdlib.h)
#endif

// clang-format on

#if defined(__arm64e__)
#define SWIFT_STDLIB_HEADER_ARCH arm64e
#elif defined(__arm64__) && defined(__ILP32__)
#define SWIFT_STDLIB_HEADER_ARCH arm64_32
#elif defined(__arm64ec__)
#define SWIFT_STDLIB_HEADER_ARCH arm64ec
#elif defined(__aarch64__) || defined(_M_ARM64)
#if defined(__APPLE__)
#define SWIFT_STDLIB_HEADER_ARCH arm64
#else
#define SWIFT_STDLIB_HEADER_ARCH aarch64
#endif
#elif defined(__x86_64h__)
#define SWIFT_STDLIB_HEADER_ARCH x86_64h
#elif defined(__x86_64__) || defined(_M_X64)
#define SWIFT_STDLIB_HEADER_ARCH x86_64
#elif defined(__i386__) || defined(_M_IX86)
#if defined(__APPLE__)
#define SWIFT_STDLIB_HEADER_ARCH i386
#else
#define SWIFT_STDLIB_HEADER_ARCH i686
#endif
#elif defined(__ARM_ARCH_7K__)
#define SWIFT_STDLIB_HEADER_ARCH armv7k
#elif defined(__ARM_ARCH_7S__)
#define SWIFT_STDLIB_HEADER_ARCH armv7s
#elif defined(__arm__) && __ARM_ARCH == 7
#define SWIFT_STDLIB_HEADER_ARCH armv7
#elif defined(__arm__) && __ARM_ARCH == 6
#define SWIFT_STDLIB_HEADER_ARCH armv6
#elif defined(__arm__) && __ARM_ARCH == 5
#define SWIFT_STDLIB_HEADER_ARCH armv5
#elif defined(__powerpc64__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define SWIFT_STDLIB_HEADER_ARCH powerpc64le
#elif defined(__powerpc64__)
#define SWIFT_STDLIB_HEADER_ARCH powerpc64
#elif defined(__powerpc__)
#define SWIFT_STDLIB_HEADER_ARCH powerpc
#elif defined(__riscv) && __riscv_xlen == 64
#define SWIFT_STDLIB_HEADER_ARCH riscv64
#elif defined(__s390x__)
#define SWIFT_STDLIB_HEADER_ARCH s390x
#elif defined(__mips64) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define SWIFT_STDLIB_HEADER_ARCH mips64el
#elif defined(__mips__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define SWIFT_STDLIB_HEADER_ARCH mipsel
#elif defined(__wasm32__)
#define SWIFT_STDLIB_HEADER_ARCH wasm32
#elif defined(__wasm64__)
#define SWIFT_STDLIB_HEADER_ARCH wasm64
#endif

#define SWIFT_STDLIB_HEADER_PATH_IMPL(path) #path
#if defined(SWIFT_STDLIB_HEADER_PLATFORM) && defined(SWIFT_STDLIB_HEADER_ARCH)
#if SWIFT_STDLIB_HAS_INCLUDE(SWIFT_STDLIB_HEADER_PLATFORM(                     \
                                 SWIFT_STDLIB_HEADER_ARCH))
#include SWIFT_STDLIB_HEADER_PLATFORM(SWIFT_STDLIB_HEADER_ARCH)
#define SWIFT_STDLIB_HEADER_FOUND
#elif defined(__x86_64h__) &&                                                  \
    SWIFT_STDLIB_HAS_INCLUDE(SWIFT_STDLIB_HEADER_PLATFORM(x86_64))
// Haswell clients can use the baseline x86_64 ABI when no specialized runtime
// bindings were installed.
#include SWIFT_STDLIB_HEADER_PLATFORM(x86_64)
#define SWIFT_STDLIB_HEADER_FOUND
#endif
#endif

#ifndef SWIFT_STDLIB_HEADER_FOUND
#error "Swift standard library C++ bindings are not installed for this target"
#endif

#undef SWIFT_STDLIB_TARGET_OS
#undef SWIFT_STDLIB_TARGET_ENVIRONMENT
#undef SWIFT_STDLIB_TARGET_VENDOR
#undef SWIFT_STDLIB_HAS_INCLUDE
#undef SWIFT_STDLIB_HEADER_FOUND
#undef SWIFT_STDLIB_HEADER_PATH_IMPL
#undef SWIFT_STDLIB_HEADER_ARCH
#undef SWIFT_STDLIB_HEADER_PLATFORM

#endif // SWIFT_PUBLIC_SWIFT_H
