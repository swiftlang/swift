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

typedef __swift_ptrdiff_t swift_tls_key_t;

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
 * Major version number for the Swift Platform Abstraction Layer.
 *
 * The major version number will be increased when there is a breaking change to
 * existing APIs.
 */
#define EMBEDDED_SWIFT_PLATFORM_VERSION_MAJOR 1

/**
 * Minor version number for the Swift Platform Abstraction Layer.
 *
 * The minor version number will be increased when new APIs or flags are
 * introduced in a manner that is not breaking for existing clients. The
 * entrypoints might be optional, where the entrypoint is needed only when
 * certain Swift functionality is used.
 */
#define EMBEDDED_SWIFT_PLATFORM_VERSION_MINOR 1

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * The number of pointer-size words that will be used to store a Mutex (as
 * provided by the Synchronization library).
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
 * Determine the version of the platform abstraction layer that the Embedded
 * Swift library was built with.
 *
 * - Parameters:
 *   - major: The value of EMBEDDED_SWIFT_PLATFORM_VERSION_MAJOR that the
 *     Swift standard library was built with.
 *   - minor: The value of EMBEDDED_SWIFT_PLATFORM_VERSION_MINOR that the
 *     Swift standard library was built with.
 *
 * This function is provided by the Swift standard library, and declared here
 * for the convenience of C clients.
 */
void swift_getPlatformLayerVersion(
  __swift_ptrdiff_t * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SINGLE major,
  __swift_ptrdiff_t * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SINGLE minor);

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
  SWIFT_MUTEX_CHECKED EMBEDDED_SWIFT_NAME(checked) = 0x01,

  /**
   * Allow the same execution context to acquire the mutex recursively.
   */
  SWIFT_MUTEX_RECURSIVE EMBEDDED_SWIFT_NAME(recursive) = 0x02
} swift_mutex_flags_t EMBEDDED_SWIFT_NAME(SwiftMutexFlags);

/**
 * Allocates memory and returns the resulting pointer.
 *
 * - Parameters:
 *   - alignment: The minimum alignment of the resulting pointer, which must
 *     be a power of at least as large as `sizeof(void *)`.
 *   - size: The minimum number of bytes to allocate.
 *   - flags: Flags to control the behavior of the allocation.
 *
 * - Returns: The allocated pointer, or NULL on failure.
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
 * Deallocates the memory referenced by `ptr`.
 *
 * - Parameters:
 *   - ptr: The pointer to be deallocated. If it is NULL, the operation does
 *     nothing.
 *   - alignment: The minimum alignment of the resulting pointer, which must
 *     be a power of at least as large as `sizeof(void *)`, or be zero to
 *     indicate that the alignment is not known.
 *   - size: The number of allocated bytes, which may be -1 if it is not
 *     known.
 *   - flags: Flags to control the behavior of the deallocation.
 *
 * This function is required when using any Embedded Swift facility that
 * requires memory allocation from the heap, whether explicitly (e.g., via the
 * `allocate` operation on unsafe pointers) or implicitly (e.g., creating a
 * copy-on-write array or an instance of a class type).
 *
 * This function can be implemented as a direct call to `free`.
 */
void _swift_deallocate(void * EMBEDDED_SWIFT_NONNULL ptr, __swift_size_t alignment, __swift_size_t size, swift_dealloc_flags_t flags);

/**
 * Allocates memory with a given type and returns the resulting pointer.
 *
 * - Parameters:
 *   - size: The minimum number of bytes to allocate.
 *   - alignment: The minimum alignment of the resulting pointer, which must
 *     be a power of at least as large as `sizeof(void *)`.
 *   - flags: Flags to control the behavior of the allocation.
 *   - typeId: An identifier used by a typed allocator to e.g. place the
 *     allocation in a particular bucket.
 *
 * - Returns: The allocated pointer, or NULL on failure.
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
 * - Parameters:
 *   - chars: The UTF-8 code points to standard output. It is not
 *     NULL-terminated.
 *   - count: The number of UTF-8 code points.
 *
 * - Returns: The number of characters that were written.
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
 * - Parameters:
 *   - buffer: The buffer into which the random bytes should be generated.
 *   - nbytes: The number of bytes that should be generated into the buffer.
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
 * - Parameters:
 *   - buffer: The buffer into which the random bytes should be generated.
 *   - nbytes: The number of bytes that should be generated into the buffer.
 *
 * This function is required when using Swift's hashed collections, such as Set
 * and Dictionary, to provide random seeding for the hash functions. Random
 * seeding makes hash values differ from one execute to the next, mitigating
 * against denial-of-service attacks that target a known hash function. The
 * random number generator provided here need not be cryptographically
 * secure. An implementation may choose to provide constant values rather than a
 * random seed to make hashing deterministic.
 *
 * This function can be implemented as a direct call to `arc4random_buf`.
 */
void _swift_generateRandomHashSeed(void * EMBEDDED_SWIFT_NONNULL EMBEDDED_SWIFT_SIZED_BY(nbytes) buffer, __swift_size_t nbytes);

/**
 * Retrieve a pointer that will be used to retain information needed for Swift's
 * dynamic exclusivity checking.
 *
 * - Returns: The pointer most recently passed to `_swift_setExclusivityTLS` on
 *   this thread. If `_swift_setExclusivityTLS` has not been called on this
 *   thread, returns NULL.
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
 * - Parameters:
 *   - ptr: The pointer to set. A subsequent call to
 *     `_swift_getExclusivityTLS` on the same thread (without an intervening
 *     call to `_swift_setExclusivityTLS`) shall return `ptr`.
 *
 * See `_swift_getExclusivityTLS` for more information about dynamic exclusivity
 * checking.
 */
void _swift_setExclusivityTLS(void * EMBEDDED_SWIFT_NULLABLE ptr);

/**
 * Initializes a mutex.
 *
 * - Parameters:
 *   - mutex: Opaque caller-owned mutex storage initialized by this function
 *     and later passed to the other `_swift_mutex_*` functions. The contents
 *     are private to the platform implementation. The storage is at least
 *     EMBEDDED_SWIFT_MUTEX_NUM_WORDS pointer-sized words and has pointer
 *     alignment.
 *   - flags: Flags controlling mutex behavior.
 *
 * This function is required when using Synchronization.Mutex.
 */
void _swift_mutex_init(void * EMBEDDED_SWIFT_NONNULL mutex,
                       swift_mutex_flags_t flags);

/**
 * Destroys a mutex initialized by `_swift_mutex_init`.
 *
 * - Parameters:
 *   - mutex: The mutex to destroy. Must not be locked.
 */
void _swift_mutex_destroy(void * EMBEDDED_SWIFT_NONNULL mutex);

/**
 * Acquires a mutex, blocking or spinning until ownership is obtained.
 *
 * - Parameters:
 *   - mutex: The mutex to acquire.
 */
void _swift_mutex_lock(void * EMBEDDED_SWIFT_NONNULL mutex);

/**
 * Releases a mutex held by the current execution context.
 *
 * - Parameters:
 *   - mutex: The mutex to release.
 */
void _swift_mutex_unlock(void * EMBEDDED_SWIFT_NONNULL mutex);

/**
 * Attempts to acquire a mutex without blocking.
 *
 * - Parameters:
 *   - mutex: The mutex to attempt to acquire.
 *
 * - Returns: Nonzero if the mutex was acquired, or zero if it was not acquired.
 */
__swift_ptrdiff_t _swift_mutex_tryLock(void * EMBEDDED_SWIFT_NONNULL mutex);

/**
 * Initializes a reserved TLS key. `key` is one of the numeric reserved keys
 * described by `SWIFT_TLS_KEY_COUNT`. `destructor` may be NULL. This function
 * is called at most once for each key that needs a destructor.
 */
void _swift_tls_init(swift_tls_key_t key,
                     __swift_tls_dtor_t EMBEDDED_SWIFT_NULLABLE destructor);

/**
 * Returns the value stored for a TLS key in the current execution context, or
 * NULL if no value has been stored.
 *
 * This function may be called before `_swift_tls_init`. In that case, it must
 * return NULL until a value is stored for the key.
 *
 * Precondition: `key < SWIFT_TLS_KEY_COUNT`.
 */
void * EMBEDDED_SWIFT_NULLABLE _swift_tls_get(swift_tls_key_t key);

/**
 * Stores a value for a TLS key in the current execution context.
 *
 * This function may be called before `_swift_tls_init`. The platform must make
 * storage available for the key on demand in that case.
 *
 * Precondition: `key < SWIFT_TLS_KEY_COUNT`.
 */
void _swift_tls_set(swift_tls_key_t key,
                    void * EMBEDDED_SWIFT_NULLABLE value);

/**
 * Returns nonzero when the current execution context is the platform's main
 * execution context.
 */
__swift_ptrdiff_t _swift_thread_isMain(void);

/**
 * Exit the program.
 *
 * - Parameters:
 *   - code: The exit code, which is typically 0 for normal termination.
 *
 * This function must not return.
 *
 * This function can be implemented directly with a call to the POSIX exit()
 * function.
 */
void _swift_exit(__swift_ptrdiff_t code);

#if defined(__cplusplus)
}
#endif

#undef EMBEDDED_SWIFT_NAME
#undef EMBEDDED_SWIFT_OPTION_SET

#endif
