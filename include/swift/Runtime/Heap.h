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
#include <cstdint>
#include <cstdlib>
#include <new>
#include <type_traits>
#include <utility>

#include "swift/Runtime/Config.h"

#if defined(_WIN32)
#include <malloc.h>
#endif

namespace swift {
// Allocate plain old memory. This is the generalized entry point
// Never returns nil. The returned memory is uninitialized. 
//
// An "alignment mask" is just the alignment (a power of 2) minus 1.
SWIFT_RUNTIME_EXPORT
void *swift_slowAlloc(size_t bytes, size_t alignMask);

// If the caller cannot promise to zero the object during destruction,
// then call these corresponding APIs:
SWIFT_RUNTIME_EXPORT
void swift_slowDealloc(void *ptr, size_t bytes, size_t alignMask);

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

namespace {
// This is C++17 and newer, so we simply re-define it.  Since the codebase is
// C++14, assume that DR1558 is accounted for and that unused parameters in alias
// templates are guaranteed to ensure SFINAE and are not ignored.
template <typename ...>
using void_t = void;

template <typename T, typename = void>
struct is_aligned_alloc_aware : std::false_type {};

template <typename T>
struct is_aligned_alloc_aware<T, void_t<decltype(T::operator new(0))>>
    : std::true_type {};
}

template <std::size_t Alignment_>
struct requires_aligned_alloc {
#if defined(__cpp_aligned_new)
  // If we have C++17 or newer we can use the alignment aware allocation
  // implicitly.
  static constexpr const bool value = false;
#else
#if defined(__STDCPP_DEFAULT_NEW_ALIGNMENT__)
  static constexpr const bool value =
      Alignment_ > std::alignment_of<std::max_align_t>::value &&
      Alignment_ > __STDCPP_DEFAULT_NEW_ALIGNMENT__;
#else
  static constexpr const bool value =
      Alignment_ > std::alignment_of<std::max_align_t>::value;
#endif
#endif
};

template <std::size_t Alignment_,
          bool = requires_aligned_alloc<Alignment_>::value>
struct aligned_alloc;

template <std::size_t Alignment_>
struct aligned_alloc<Alignment_, false> {};

template <std::size_t Alignment_>
struct aligned_alloc<Alignment_, true> {
  [[nodiscard]] void *operator new(std::size_t size) noexcept {
#if defined(_WIN32)
    return _aligned_malloc(size, Alignment_);
#else
    static_assert(Alignment_ >= sizeof(void *),
                  "posix_memalign requires minimal alignment of pointer");
    void *ptr = nullptr;
    (void)posix_memalign(&ptr, Alignment_, size);
    return ptr;
#endif
  }

  void operator delete(void *ptr) noexcept {
#if defined(_WIN32)
    _aligned_free(ptr);
#else
    free(ptr);
#endif
  }

#if defined(_WIN32)
  // FIXME: why is this even needed?  This is not permitted as per the C++
  // standard new.delete.placement (§17.6.3.4).
  [[nodiscard]] void *operator new(std::size_t size, void *where) noexcept {
    return ::operator new(size, where);
  }
#endif
};
}

#endif // SWIFT_RUNTIME_HEAP_H
