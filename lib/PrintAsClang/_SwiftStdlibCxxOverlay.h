//===--- _SwiftStlibCxxOverlay.h - Additions for Stdlib ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef __cplusplus
#error "no C++"
#endif

#ifdef SWIFT_CXX_INTEROP_OPTIONAL_MIXIN

/// True when the Optional has a value.
SWIFT_INLINE_THUNK operator bool() const noexcept { return *this != none; }

/// Returns the value stored in the Optional.
///
/// The returned value is copied using the appropriate Swift / C++ copy
/// semantics.
SWIFT_INLINE_THUNK T_0_0 get() const
    noexcept(noexcept(getUnsafelyUnwrapped())) {
  // FIXME: Fail with source location.
  return getUnsafelyUnwrapped();
}

#undef SWIFT_CXX_INTEROP_OPTIONAL_MIXIN

#else
// out-of-class overlay for Swift standard library.

static_assert(sizeof(_impl::_impl_String) >= 0,
              "included outside of stdlib bindings");

namespace cxxOverlay {

class IterationEndSentinel;

/// Abstract Swift collection iterator.
template <class Collection, class T> class CollectionIterator {
public:
  using Index =
      decltype(reinterpret_cast<Collection *>(0x123)->getStartIndex());

  SWIFT_INLINE_THUNK CollectionIterator(const Collection &collection)
      : collection(collection) {
    index = collection.getStartIndex();
    endIndex = collection.getEndIndex();
    // FIXME: Begin read access.
  }

  SWIFT_INLINE_THUNK ~CollectionIterator() {
    // FIXME: End read access.
  }

  SWIFT_INLINE_THUNK T operator*() const { return collection[index]; }
  SWIFT_INLINE_THUNK void operator++() {
    ++index;
    // FIXME: assert(index <= endIndex); // No need to go past the end.
  }

  SWIFT_INLINE_THUNK bool operator!=(const IterationEndSentinel &) const {
    return index != endIndex;
  }

private:
  Index index, endIndex;
  const Collection &collection;
};

class IterationEndSentinel {};

template <class T> using ArrayIterator = CollectionIterator<Array<T>, T>;

} // namespace cxxOverlay

// FIXME: This should apply to more than the Array type.
template <class T>
SWIFT_INLINE_THUNK cxxOverlay::ArrayIterator<T> begin(const Array<T> &array
                                          [[clang::lifetimebound]]) {
  return cxxOverlay::ArrayIterator<T>(array);
}

template <class T>
SWIFT_INLINE_THUNK cxxOverlay::IterationEndSentinel end(const Array<T> &) {
  return {};
}

#endif
