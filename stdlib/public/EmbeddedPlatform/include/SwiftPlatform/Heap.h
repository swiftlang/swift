/*===-------------- Embedded Swift Platform Heap Allocation --------*- C -*-===*
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
 * This file provides declarations for the heap allocation functions that a
 * platform needs to provide to support Embedded Swift.
 *
 *===----------------------------------------------------------------------===*/

#ifndef SWIFT_PLATFORM_HEAP_H
#define SWIFT_PLATFORM_HEAP_H

#include "Types.h"

#if defined(__cplusplus)
extern "C" {
#endif

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
 *     be a power of two at least as large as `sizeof(void *)`, or be zero to
 *     request the default alignment of 16 bytes.
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
 * Deallocates the memory referenced by `ptr` with a given type
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
 *   - typeId: An identifier used by a typed allocator to e.g. place the
 *     allocation in a particular bucket.
 *
 * This function is required when using any Embedded Swift facility that
 * requires memory allocation from the heap, whether explicitly (e.g., via the
 * `allocate` operation on unsafe pointers) or implicitly (e.g., creating a
 * copy-on-write array or an instance of a class type).
 *
 * This function can be implemented as a direct call to `free`.
 */
void _swift_typedDeallocate(void * EMBEDDED_SWIFT_NONNULL ptr, __swift_size_t size, __swift_size_t alignment, swift_dealloc_flags_t flags, __swift_typeid_t typeId);

#if defined(__cplusplus)
}
#endif

#endif
