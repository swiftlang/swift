//===--- Heap.h - Swift Language Heap ABI -----------------------*- C++ -*-===//
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
//
// Swift Heap ABI
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_HEAP_H
#define SWIFT_RUNTIME_HEAP_H

#include <cstddef>
#include <new>
#include <utility>

#include "swift/Runtime/Config.h"
#include "swift/shims/Visibility.h"

namespace swift {
// Allocate plain old memory. This is the generalized entry point
// Never returns nil. The returned memory is uninitialized. 
//
// An "alignment mask" is just the alignment (a power of 2) minus 1.
SWIFT_EXTERN_C SWIFT_RETURNS_NONNULL SWIFT_NODISCARD SWIFT_RUNTIME_EXPORT_ATTRIBUTE
void *swift_slowAlloc(size_t bytes, size_t alignMask);

using MallocTypeId = unsigned long long;

SWIFT_RETURNS_NONNULL SWIFT_NODISCARD
void *swift_slowAllocTyped(size_t bytes, size_t alignMask, MallocTypeId typeId);

// If the caller cannot promise to zero the object during destruction,
// then call these corresponding APIs:
SWIFT_RUNTIME_EXPORT
void swift_slowDealloc(void *ptr, size_t bytes, size_t alignMask);

SWIFT_RUNTIME_EXPORT
void swift_clearSensitive(void *ptr, size_t bytes);

/// Allocate and construct an instance of type \c T.
///
/// \param args The arguments to pass to the constructor for \c T.
///
/// \returns A pointer to a new, fully constructed instance of \c T. This
///   function never returns \c nullptr. The caller is responsible for
///   eventually destroying the resulting object by passing it to
///   \c swift_cxx_deleteObject().
///
/// This function avoids the use of the global \c operator \c new (which may be
/// overridden by other code in a process) in favor of calling
/// \c swift_slowAlloc() and constructing the new object with placement new.
///
/// This function is capable of returning well-aligned memory even on platforms
/// that do not implement the C++17 "over-aligned new" feature.
template <typename T, typename... Args>
SWIFT_RETURNS_NONNULL SWIFT_NODISCARD
static inline T *swift_cxx_newObject(Args &&... args) {
  auto result = reinterpret_cast<T *>(swift_slowAlloc(sizeof(T),
                                                      alignof(T) - 1));
  ::new (result) T(std::forward<Args>(args)...);
  return result;
}

/// Destruct and deallocate an instance of type \c T.
///
/// \param ptr A pointer to an instance of type \c T previously created with a
///   call to \c swift_cxx_newObject().
///
/// This function avoids the use of the global \c operator \c delete (which may
/// be overridden by other code in a process) in favor of directly calling the
/// destructor for \a *ptr and then freeing its memory by calling
/// \c swift_slowDealloc().
///
/// The effect of passing a pointer to this function that was \em not returned
/// from \c swift_cxx_newObject() is undefined.
template <typename T>
static inline void swift_cxx_deleteObject(T *ptr) {
  if (ptr) {
    ptr->~T();
    swift_slowDealloc(ptr, sizeof(T), alignof(T) - 1);
  }
}
}

#endif // SWIFT_RUNTIME_HEAP_H
