/*===------------------ Embedded Swift Platform Declarations ------*- C -*-===*
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
 * This file provides a description of the functions that a platform is expected
 * to provide to work with Embedded Swift. A "platform" in this case is broadly
 * construed, and can be anything from a couple of function implementations
 * within a standalone Embedded Swift program to a full-fledged SDK for an
 * OS. Either way, any of these functions can be implemented either in Swift
 * (using `@c`) or in C and linked into the final Embedded Swift executable.
 *
 * Many of these functions precisely match the signatures of functions defined
 * by POSIX. This is deliberate, because it allows simple pass-through
 * implementations for POSIX systems.
 *
 *===----------------------------------------------------------------------===*/

#ifndef EMBEDDED_SWIFT_PLATFORM_H
#define EMBEDDED_SWIFT_PLATFORM_H

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
 * 64-bit type  information that is used for typed allocation and
 * deallocation.
 */
typedef unsigned long long __swift_options_t;

#if __has_feature(nullability)
#define EMBEDDED_SWIFT_NONNULL _Nonnull
#define EMBEDDED_SWIFT_NULLABLE _Nullable
#else
#define EMBEDDED_SWIFT_NONNULL
#define EMBEDDED_SWIFT_NULLABLE
#endif

#if defined(__has_feature) && (__has_feature(bounds_attributes) || __has_feature(bounds_safety_attributes))
#define EMBEDDED_SWIFT_COUNTED_BY(N) __attribute__((__counted_by__(N)))
#define EMBEDDED_SWIFT_SIZED_BY(N) __attribute__((__sized_by__(N)))
#define EMBEDDED_SWIFT_SINGLE __attribute__((__single__))
#else
#define EMBEDDED_SWIFT_COUNTED_BY(N)
#define EMBEDDED_SWIFT_SIZED_BY(N)
#define EMBEDDED_SWIFT_SINGLE
#endif

#if defined(__has_feature) && __has_attribute(flag_enum)
#define EMBEDDED_SWIFT_OPTION_SET __attribute__((flag_enum,enum_extensibility(open)))
#else
#define EMBEDDED_SWIFT_OPTION_SET
#endif

#if defined(__has_feature) && __has_attribute(swift_name)
#define EMBEDDED_SWIFT_NAME(_name) __attribute__((swift_name(#_name)))
#else
#define EMBEDDED_SWIFT_NAME(_name)
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
  SWIFT_FREE_NONE EMBEDDED_SWIFT_NAME(none) = 0,
} swift_free_flags_t EMBEDDED_SWIFT_NAME(SwiftFreeFlags);

/**
 * Allocates memory and returns the resulting pointer.
 *
 * Parameters:
 *   - `size`: the minimum number of bytes to allocate.
 *   - `alignment`: the minimum alignment of the resulting pointer, which must
 *     be a power of at least as large as `sizeof(void *)`.
 *   - `flags`: flags to control the behavior of the allocation.
 *
 * Returns the allocated pointer, or NULL on failure.
 *
 * This function is required when using any Embedded Swift facility that
 * requires memory allocation from the heap, whether explicitly (e.g., via the
 * `allocate` operation on unsafe pointers) or implicitly (e.g., creating a
 * copy-on-write array or an instance of a class type).
 * 
 * This function can be implemented as a call to `posix_memalign`.
 */
void * EMBEDDED_SWIFT_NULLABLE _swift_allocate(__swift_size_t alignment, __swift_size_t size, swift_alloc_flags_t flags);

/**
 * Frees the memory referenced by `ptr`.
 *
 * Parameters:
 *   - `ptr`: The pointer to be freed. If it is NULL, the operation does
 *     nothing.
 *   - `size`: the number of allocated bytes, which may be -1 if it is not
 *     known.
 *   - `alignment`: the minimum alignment of the resulting pointer, which must
 *     be a power of at least as large as `sizeof(void *)`, or be zero to
 *     indicate that the alignment is not known.
 *   - `flags`: flags to control the behavior of the free.
 *
 * This function is required when using any Embedded Swift facility that
 * requires memory allocation from the heap, whether explicitly (e.g., via the
 * `allocate` operation on unsafe pointers) or implicitly (e.g., creating a
 * copy-on-write array or an instance of a class type).
 * 
 * This function can be implemented as a direct call to `free`.
 */
void _swift_free(void * EMBEDDED_SWIFT_NONNULL ptr, __swift_size_t alignment, __swift_size_t size, swift_free_flags_t flags);

/**
 * Allocates memory with a given type and returns the resulting pointer.
 *
 * Parameters:
 *   - `size`: the minimum number of bytes to allocate.
 *   - `alignment`: the minimum alignment of the resulting pointer, which must
 *     be a power of at least as large as `sizeof(void *)`.
 *   - `flags`: flags to control the behavior of the allocation.
 *   - `typeId`: an identifier used by a typed allocator to e.g. place the
 *     allocation in a particular bucket.
 *
 * Returns the allocated pointer, or NULL on failure.
 *
 * This function is required when using any Embedded Swift facility that
 * requires typed memory allocation from the heap, e.g. class instance
 * allocations when the TypedAllocation feature is enabled.
 *
 * This function can be implemented as a direct call to `posix_memalign`,
 * if the target platform does not support typed allocations.
 */
void * EMBEDDED_SWIFT_NULLABLE _swift_typedAllocate(
    __swift_size_t size, __swift_size_t alignment, swift_alloc_flags_t flags, __swift_typeid_t typeId);

/**
 * Writes a sequence of UTF-8 code points to standard output.
 *
 * Parameters:
 *   - `chars`: the UTF-8 code points to standard output. It is not
 *     NULL-terminated.
 *   - `count`: the number of UTF-8 code points.
 *
 * Returns the number of characters that were written.
 *
 * This function is required when using the Embedded Swift print() facilities.
 *
 * This function can be implemented as a call to fwrite or printf with the
 * specified number of code points.
 */
__swift_size_t _swift_writeToStandardOutput(
    const unsigned char * EMBEDDED_SWIFT_NULLABLE EMBEDDED_SWIFT_COUNTED_BY(count) chars,
    __swift_size_t count);

/**
 * Generates random bytes into the given buffer.
 *
 * Parameters:
 *   - `buffer`: the buffer into which the random bytes should be generated.
 *   - `nbytes`: the number of bytes that should be generated into the buffer.
 *
 * This function is required when using Swift's SystemRandomNumberGenerator, the
 * default random number generator used for shuffling elements and producing
 * random values. While this function is encouraged to use a cryptographically
 * secure algorithm, it is not required to do so.
 *
 * Note that this function is not used to provide random seeding for the hash
 * functions used in Set and Dictionary. Those operations use
 * `_swift_generateRandomHashSeed`.
 *
 * This function can be implemented as a direct call to `arc4random_buf`.
 */
void _swift_generateRandom(void * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SIZED_BY(nbytes) buffer, __swift_size_t nbytes);

/**
 * Generates random bytes intended for a hashing seed into the given buffer.
 *
 * Parameters:
 *   - `buffer`: the buffer into which the random bytes should be generated.
 *   - `nbytes`: the number of bytes that should be generated into the buffer.
 *
 * This function is required when using Swift's hashed collections, such as Set
 * and Dictionary, to provide random seeding for the hash functions. Random
 * seeding makes hash values differ from one execute to the next, mitigating
 * against denial-of-service attacks that target a known hash function. The
 * random number generator provided here need not be cryptographically
 * secure. An implementation may choose to provide constant values random than a
 * random seed to make hashing deterministic.
 *
 * This function can be implemented as a direct call to `arc4random_buf`.
 */
void _swift_generateRandomHashSeed(void * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SIZED_BY(nbytes) buffer, __swift_size_t nbytes);

/**
 * Retrieve a pointer that will be used to retain information needed for Swift's
 * dynamic exclusivity checking.
 *
 * Returns the pointer most recently passed to `_swift_setExclusivityTLS` on
 * this thread. If `_swift_setExclusivityTLS` has not been called on this
 * thread, returns NULL.
 *
 * In a single-threaded environment, the `_swift_getExclusivityTLS` and
 * `_swift_setExclusivityTLS` functions can get and set a global variable that
 * is initialized to NULL. In a multi-threaded environment, the variable will
 * need to be in thread-local storage (e.g., using C11 `_Thread_local`) or a
 * similar facility.
 *
 * This function is required when using Swift's dynamic exclusivity checking,
 * which is enabled by the Swift compiler option `-enforce-exclusivity=checked`
 * and required when the compiler cannot statically prove that all accesses to a
 * given variable (such as a global variable or a stored instance property of a
 * class) respect the exclusivity model.
 */
void * EMBEDDED_SWIFT_NULLABLE _swift_getExclusivityTLS(void);

/**
 * Set the pointer that will be used to retain information needed for Swift's
 * dynamic exclusivity checking.
 *
 * Parameters:
 *   - `ptr`: the pointer to set. A subsequent call to
 *     `_swift_getExclusivityTLS` on the same thread (without an intervening
 *     call to `_swift_setExclusivityTLS`) shall return `ptr`.
 *
 * See `_swift_getExclusivityTLS` for more information about dynamic exclusivity
 * checking.
 */
void _swift_setExclusivityTLS(void * EMBEDDED_SWIFT_NULLABLE ptr);

/**
 * Exit the program.
 *
 * Parameters:
 * - `code`: the exit code, which is typically 0 for normal termination.
 *
 * This function must not return.
 *
 * This function can be implemented directly with a call to the POSIX exit()
 * function.
 */
void _swift_exit(__swift_ptrdiff_t code);

#undef EMBEDDED_SWIFT_SINGLE
#undef EMBEDDED_SWIFT_SIZED_BY
#undef EMBEDDED_SWIFT_COUNTED_BY
#undef EMBEDDED_SWIFT_NULLABLE
#undef EMBEDDED_SWIFT_NONNULL
#undef EMBEDDED_SWIFT_NAME
#undef EMBEDDED_SWIFT_OPTION_SET

#endif
