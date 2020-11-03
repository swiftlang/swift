//===--- Compiler.h - Compiler specific definitions -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_COMPILER_H
#define SWIFT_BASIC_COMPILER_H

#if defined(_MSC_VER) && !defined(__clang__)
#define SWIFT_COMPILER_IS_MSVC 1
#else
#define SWIFT_COMPILER_IS_MSVC 0
#endif

// Workaround non-clang compilers
#ifndef __has_builtin
#define __has_builtin(x) 0
#endif
#ifndef __has_attribute
#define __has_attribute(x) 0
#endif

// __builtin_assume() is an optimization hint.
#if __has_builtin(__builtin_assume)
#define SWIFT_ASSUME(x) __builtin_assume(x)
#else
#define SWIFT_ASSUME(x)
#endif

/// Attributes.

#if __has_attribute(constructor)
#define SWIFT_CONSTRUCTOR __attribute__((constructor))
#else
#define SWIFT_CONSTRUCTOR
#endif

/// \macro SWIFT_GNUC_PREREQ
/// Extend the default __GNUC_PREREQ even if glibc's features.h isn't
/// available.
#ifndef SWIFT_GNUC_PREREQ
# if defined(__GNUC__) && defined(__GNUC_MINOR__) && defined(__GNUC_PATCHLEVEL__)
#  define SWIFT_GNUC_PREREQ(maj, min, patch) \
    ((__GNUC__ << 20) + (__GNUC_MINOR__ << 10) + __GNUC_PATCHLEVEL__ >= \
     ((maj) << 20) + ((min) << 10) + (patch))
# elif defined(__GNUC__) && defined(__GNUC_MINOR__)
#  define SWIFT_GNUC_PREREQ(maj, min, patch) \
    ((__GNUC__ << 20) + (__GNUC_MINOR__ << 10) >= ((maj) << 20) + ((min) << 10))
# else
#  define SWIFT_GNUC_PREREQ(maj, min, patch) 0
# endif
#endif


/// SWIFT_ATTRIBUTE_NOINLINE - On compilers where we have a directive to do so,
/// mark a method "not for inlining".
#if __has_attribute(noinline) || SWIFT_GNUC_PREREQ(3, 4, 0)
#define SWIFT_ATTRIBUTE_NOINLINE __attribute__((noinline))
#elif defined(_MSC_VER)
#define SWIFT_ATTRIBUTE_NOINLINE __declspec(noinline)
#else
#define SWIFT_ATTRIBUTE_NOINLINE
#endif

/// SWIFT_ATTRIBUTE_ALWAYS_INLINE - On compilers where we have a directive to do
/// so, mark a method "always inline" because it is performance sensitive. GCC
/// 3.4 supported this but is buggy in various cases and produces unimplemented
/// errors, just use it in GCC 4.0 and later.
#if __has_attribute(always_inline) || SWIFT_GNUC_PREREQ(4, 0, 0)
#define SWIFT_ATTRIBUTE_ALWAYS_INLINE __attribute__((always_inline))
#elif defined(_MSC_VER)
#define SWIFT_ATTRIBUTE_ALWAYS_INLINE __forceinline
#else
#define SWIFT_ATTRIBUTE_ALWAYS_INLINE
#endif

#ifdef __GNUC__
#define SWIFT_ATTRIBUTE_NORETURN __attribute__((noreturn))
#elif defined(_MSC_VER)
#define SWIFT_ATTRIBUTE_NORETURN __declspec(noreturn)
#else
#define SWIFT_ATTRIBUTE_NORETURN
#endif

#endif // SWIFT_BASIC_COMPILER_H
