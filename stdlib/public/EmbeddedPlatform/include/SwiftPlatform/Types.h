/*===------------------ Embedded Swift Platform Types ------*- C -*-===*
 *
 * This source file is part of the Swift.org open source project
 *
 * Copyright (c) 2026 Apple Inc. and the Swift project authors
 * Licensed under Apache License v2.0 with Runtime Library Exception
 *
 * See https: *swift.org/LICENSE.txt for license information
 * See https: *swift.org/CONTRIBUTORS.txt for the list of Swift project authors
 *
 *===----------------------------------------------------------------------=== *
 *
 * This file provides types that are used in the Embedded Swift Platform
 * Abstraction Layer. See EmbeddedPlatform.h for the main definitions in the
 * PAL.
 *
 *===----------------------------------------------------------------------===*/

#ifndef SWIFT_PLATFORM_TYPES_H
#define SWIFT_PLATFORM_TYPES_H


#if !defined(__has_feature)
#define __has_feature(x) 0
#endif

#if defined(__SIZE_TYPE__) && defined(__PTRDIFF_TYPE__)
typedef __SIZE_TYPE__ __swift_size_t;
typedef __PTRDIFF_TYPE__ __swift_ptrdiff_t;
#else
#include <stddef.h>
typedef size_t __swift_size_t;
typedef ptrdiff_t __swift_ptrdiff_t;
#endif

/**
 * 64-bit type identifier information that is used for typed allocation and
 * deallocation.
 */
typedef unsigned long long __swift_typeid_t;

/**
 * 64-bit option set used to provide options for various functions in the
 * platform abstraction layer.
 */
typedef unsigned long long __swift_options_t;

#if defined(__INT64_TYPE__)
typedef __INT64_TYPE__ __swift_int64_t;
#else
#include <stdint.h>
typedef int64_t __swift_int64_t;
#endif

typedef __swift_ptrdiff_t __swift_tls_key_t;

/**
 * Number of reserved TLS keys used by Embedded Swift runtime components.
 *
 * The numeric values are kept in sync with the reserved keys in
 * swift/Threading/TLSKeys.h. The EmbeddedPlatform TLS contract does not
 * provide dynamic key allocation; every key passed to the `_swift_tls_*`
 * functions will be one of these reserved values. The key values are dense in
 * the range `[0, SWIFT_TLS_KEY_COUNT)`, so platform implementations may use
 * them directly as array indices.
 */
#define SWIFT_TLS_KEY_COUNT 8

#if __has_feature(nullability)
#define EMBEDDED_SWIFT_NONNULL _Nonnull
#define EMBEDDED_SWIFT_NULLABLE _Nullable
#else
#define EMBEDDED_SWIFT_NONNULL
#define EMBEDDED_SWIFT_NULLABLE
#endif

/**
 * A function called with a non-NULL TLS value when the execution context that
 * owns it exits.
 *
 * A platform with a fixed set of execution contexts that never exit may never
 * call this function.
 */
typedef void (*__swift_tls_dtor_t)(void * EMBEDDED_SWIFT_NULLABLE);

#if defined(__has_feature) && (__has_feature(bounds_attributes) || __has_feature(bounds_safety_attributes))
#define EMBEDDED_SWIFT_COUNTED_BY(N) __attribute__((__counted_by__(N)))
#define EMBEDDED_SWIFT_SIZED_BY(N) __attribute__((__sized_by__(N)))
#define EMBEDDED_SWIFT_SINGLE __attribute__((__single__))
#else
#define EMBEDDED_SWIFT_COUNTED_BY(N)
#define EMBEDDED_SWIFT_SIZED_BY(N)
#define EMBEDDED_SWIFT_SINGLE
#endif

#if defined(__has_attribute) && __has_attribute(flag_enum) && __has_attribute(enum_extensibility)
#define EMBEDDED_SWIFT_OPTION_SET __attribute__((flag_enum,enum_extensibility(open)))
#else
#define EMBEDDED_SWIFT_OPTION_SET
#endif

#if defined(__has_attribute) && __has_attribute(swift_name)
#define EMBEDDED_SWIFT_NAME(_name) __attribute__((swift_name(#_name)))
#else
#define EMBEDDED_SWIFT_NAME(_name)
#endif

#if defined(__has_attribute) && __has_attribute(noreturn)
#define EMBEDDED_SWIFT_NORETURN __attribute__((noreturn))
#else
#define EMBEDDED_SWIFT_NORETURN
#endif

#if defined(__has_attribute) && __has_attribute(enum_extensibility)
#define EMBEDDED_SWIFT_ENUM __attribute__((enum_extensibility(open)))
#else
#define EMBEDDED_SWIFT_ENUM
#endif

/**
 * The number of pointer-size words that will be used to store a non-recursive
 * Mutex (as provided by the Synchronization library), i.e., one backed by the
 * `_swift_mutex_*` functions.
 *
 * This needs to be large enough to accommodate any implementation of Mutex that
 * can be implemented for that given platform (e.g., via the `_swift_mutex_*`
 * functions). It can be defined externally (via `-D` on the command line for
 * Clang, `-Xcc -D` for Swift) to a different value, but that value must be
 * consistent throughout the build to prevent ABI mismatches.
 */
#ifndef EMBEDDED_SWIFT_MUTEX_NUM_WORDS
#if defined(__APPLE__) && __SIZEOF_POINTER__ == 4
// On 32-bit Apple targets (e.g., watchOS armv7k / arm64_32) `pthread_mutex_t`
// is 40 bytes, which doesn't fit in 8 four-byte words.
#define EMBEDDED_SWIFT_MUTEX_NUM_WORDS (__swift_ptrdiff_t)12
#else
#define EMBEDDED_SWIFT_MUTEX_NUM_WORDS (__swift_ptrdiff_t)8
#endif
#endif

/**
 * The number of pointer-size words that will be used to store a recursive
 * Mutex, i.e., one backed by the `_swift_mutexRecursive_*` functions.
 *
 * This is tracked separately from EMBEDDED_SWIFT_MUTEX_NUM_WORDS because a
 * platform's recursive mutex implementation need not have the same size as
 * its non-recursive one. It can be defined externally (via `-D` on the
 * command line for Clang, `-Xcc -D` for Swift) to a different value, but that
 * value must be consistent throughout the build to prevent ABI mismatches.
 */
#ifndef EMBEDDED_SWIFT_MUTEX_RECURSIVE_NUM_WORDS
#if defined(__APPLE__) && __SIZEOF_POINTER__ == 4
// On 32-bit Apple targets (e.g., watchOS armv7k / arm64_32) `pthread_mutex_t`
// is 40 bytes, which doesn't fit in 8 four-byte words.
#define EMBEDDED_SWIFT_MUTEX_RECURSIVE_NUM_WORDS (__swift_ptrdiff_t)12
#else
#define EMBEDDED_SWIFT_MUTEX_RECURSIVE_NUM_WORDS (__swift_ptrdiff_t)8
#endif
#endif

/**
 * Options provided to the Swift memory allocation function.
 */
typedef enum EMBEDDED_SWIFT_OPTION_SET: __swift_options_t {
  /**
   * No options.
   */
  SWIFT_ALLOC_NONE EMBEDDED_SWIFT_NAME(none) = 0,

  /**
   * Zero the memory before returning it, like the C library function `calloc`.
   */
  SWIFT_ALLOC_ZERO_MEMORY EMBEDDED_SWIFT_NAME(zeroMemory) = 0x01
} swift_alloc_flags_t EMBEDDED_SWIFT_NAME(SwiftAllocateFlags);

/**
 * Options provided to the Swift memory deallocation function.
 */
typedef enum EMBEDDED_SWIFT_OPTION_SET: __swift_options_t {
  /**
   * No options.
   */
  SWIFT_DEALLOC_NONE EMBEDDED_SWIFT_NAME(none) = 0,
} swift_dealloc_flags_t EMBEDDED_SWIFT_NAME(SwiftDeallocFlags);

/**
 * The kind of error being reported.
 */
typedef enum EMBEDDED_SWIFT_ENUM {
  /**
   * A fatal error, produced by fatalError(...).
   */
  SWIFT_ERROR_FATAL EMBEDDED_SWIFT_NAME(fatal) = 0,

  /**
   * A precondition failure, produced by precondition(...).
   */
  SWIFT_ERROR_PRECONDITION EMBEDDED_SWIFT_NAME(precondition),

  /**
   * An assertion failure, produced by assert(...).
   *
   * These won't occur when assertions are disabled, e.g., in release builds.
   */
  SWIFT_ERROR_ASSERTION EMBEDDED_SWIFT_NAME(assertion),
} swift_error_kind_t EMBEDDED_SWIFT_NAME(SwiftErrorKind);

/**
 * Options provided to the Swift mutex initialization function.
 */
typedef enum EMBEDDED_SWIFT_OPTION_SET: __swift_options_t {
  /**
   * No options.
   */
  SWIFT_MUTEX_NONE EMBEDDED_SWIFT_NAME(none) = 0,

  /**
   * Diagnose mutex misuse when the platform can do so cheaply.
   */
  SWIFT_MUTEX_CHECKED EMBEDDED_SWIFT_NAME(checked) = 0x01
} swift_mutex_flags_t EMBEDDED_SWIFT_NAME(SwiftMutexFlags);

#endif
