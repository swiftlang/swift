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
#include <limits>
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

// If SWIFT_STDLIB_HAS_MALLOC_TYPE is defined, allocate typed memory.
// Otherwise, allocate plain memory.
SWIFT_RUNTIME_EXPORT
void *swift_coroFrameAlloc(size_t bytes, MallocTypeId typeId);

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

/// Define a custom operator delete; this is useful when a class has a
/// virtual destructor, as in that case the compiler will emit a deleting
/// version of the destructor, which will call ::operator delete unless the
/// class (or its superclasses) define one of their own.
#define SWIFT_CXX_DELETE_OPERATOR(T)                    \
  void operator delete(void *ptr) {                     \
    swift_slowDealloc(ptr, sizeof(T), alignof(T) - 1);  \
  }

/// A C++ Allocator that uses the above functions instead of operator new
/// and operator delete.  This lets us use STL containers without pulling
/// in global operator new and global operator delete.
template <typename T>
struct cxx_allocator {
  // Member types
  typedef T              value_type;
  typedef T             *pointer;
  typedef const T       *const_pointer;
  typedef T             &reference;
  typedef const T       &const_reference;
  typedef std::size_t    size_type;
  typedef std::ptrdiff_t difference_type;
  typedef std::true_type propagate_on_container_move_assignment;
  typedef std::true_type is_always_equal;

  template <typename U>
  struct rebind {
    typedef cxx_allocator<U> other;
  };

  cxx_allocator() noexcept {}
  cxx_allocator(const cxx_allocator &other) noexcept { (void)other; }

  template <class U>
  cxx_allocator(const cxx_allocator<U> &other) noexcept { (void)other; }

  ~cxx_allocator() {}

  pointer address(reference x) const noexcept {
    return reinterpret_cast<pointer>(&reinterpret_cast<volatile char &>(x));
  }

  const_pointer address(const_reference x) const noexcept {
    return reinterpret_cast<const_pointer>(&
      reinterpret_cast<const volatile char &>(x));
  }

  T *allocate(std::size_t n) {
    return reinterpret_cast<T *>(swift_slowAlloc(sizeof(T) * n,
                                                 alignof(T) - 1));
  }
  T *allocate(std::size_t n, const void *hint) {
    (void)hint;
    return allocate(n);
  }

  void deallocate(T *p, std::size_t n) {
    swift_slowDealloc(p, sizeof(T) * n, alignof(T) - 1);
  }

  size_type max_size() const noexcept {
    return std::numeric_limits<size_type>::max() / sizeof(T);
  }

  template <class U, class... Args>
  void construct(U *p, Args&&... args) {
    ::new((void *)p) U(std::forward<Args>(args)...);
  }

  template <class U>
  void destroy(U *p) {
    p->~U();
  }
};

template <typename T, typename U>
bool operator==(const cxx_allocator<T> &lhs,
                const cxx_allocator<U> &rhs) noexcept {
  return true;
}

template <typename T, typename U>
bool operator!=(const cxx_allocator<T> &lha,
                const cxx_allocator<U> &rhs) noexcept {
  return false;
}

}

#endif // SWIFT_RUNTIME_HEAP_H
