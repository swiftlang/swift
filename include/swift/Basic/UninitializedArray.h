//===--- UninitializedArray.h - Array of uninitialized objects --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
///  This file defines the UninitializedArray "data structure", which
///  can hold an uninitialized array of values and provides explicit
///  operations to copy, move, and destroy them.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_UNINITIALIZEDARRAY_H
#define SWIFT_BASIC_UNINITIALIZEDARRAY_H

#include <assert.h>
#include <memory>

namespace swift {

/// An array of uninitialized elements.  The user is responsible for
/// ensuring that it's used properly.
template <class T, size_t N>
class UninitializedArray {
  union {
    T elements[N];
  };

public:
  UninitializedArray() {}
  UninitializedArray(const UninitializedArray &other) = delete;
  UninitializedArray &operator=(const UninitializedArray &other) = delete;
  UninitializedArray(UninitializedArray &&other) = delete;
  UninitializedArray &operator=(UninitializedArray &&other) = delete;
  ~UninitializedArray() {}

  using iterator = T *;
  using const_iterator = const T *;
  iterator begin() { return elements; }
  const_iterator begin() const { return elements; }
  // We intentionally don't provide end() because it's too easy to use it
  // accidentally when there's no guarantee that those elements exist.

  template <class... Args>
  T &emplace(size_t i, Args &&...args) {
    assert(i < N);
    return *::new ((void*) &elements[i]) T(std::forward<Args>(args)...);
  }

  T &operator[](size_t i) {
    assert(i < N);
    return elements[i];
  }

  const T &operator[](size_t i) const {
    assert(i < N);
    return elements[i];
  }

  /// Given that this array contains no initialized elements and the other
  /// array contains at least newSize initialized elements, fill this array
  /// with newSize initialized elements copied from the other array.
  void copyInitialize(const UninitializedArray &other, size_t newSize) {
    assert(newSize <= N);
    std::uninitialized_copy(other.begin(), other.begin() + newSize, begin());
  }

  /// Given that this array contains oldSize initialized elements and the other
  /// array contains at least newSize initialized elements, fill this array
  /// with newSize initialized elements copied from the other array.
  void copyAssign(const UninitializedArray &other,
                  size_t oldSize, size_t newSize) {
    assert(oldSize <= N);
    assert(newSize <= N);

    auto commonSize = std::min(oldSize, newSize);
    auto thisBegin = begin();
    auto otherBegin = other.begin();

    // Copy-assign the common prefix.
    std::copy(otherBegin, otherBegin + commonSize, thisBegin);

    // If there are more elements in the other array, copy-initialize those
    // elements into this array starting after the common prefix.
    if (oldSize < newSize) {
      std::uninitialized_copy(otherBegin + commonSize, otherBegin + newSize,
                              thisBegin + commonSize);

    // Otherwise, if there were more elements in this array, destroy the
    // excess elements.
    } else if (oldSize > newSize) {
      std::destroy(thisBegin + commonSize, thisBegin + oldSize);
    }
  }

  /// Given that this array contains no initialized elements and the other
  /// array contains exactly newSize initialized elements, fill this array
  /// with newSize initialized elements destructively moved from the other
  /// array.  The other array is left with no initialized elements.
  void destructiveMoveInitialize(UninitializedArray &&other, size_t newSize) {
    assert(newSize <= N);
    auto it = std::move_iterator(other.begin());
    std::uninitialized_copy(it, it + newSize, begin());
    std::destroy(other.begin(), other.begin() + newSize);
  }

  /// Given that this array contains oldSize initialized elements and the other
  /// array contains exactly newSize initialized elements, fill this array with
  /// newSize initialized elements destructively moved from the other array.
  /// The other array is left with no initialized elements.
  void destructiveMoveAssign(UninitializedArray &&other,
                             size_t oldSize, size_t newSize) {
    assert(oldSize <= N);
    assert(newSize <= N);

    auto commonSize = std::min(oldSize, newSize);
    auto thisBegin = begin();
    auto otherBegin = std::move_iterator(other.begin());

    // Move-assign the common prefix.  Note that we use a move_iterator to
    // cause all these "copies" to be moves.
    std::copy(otherBegin, otherBegin + commonSize, thisBegin);

    // If there are more elements in the new array, move-initialize those
    // elements starting after the common prefix.
    if (oldSize < newSize) {
      std::uninitialized_copy(otherBegin + commonSize, otherBegin + newSize,
                              thisBegin + commonSize);

    // Otherwise, if there were more elements in this array, destroy the
    // excess elements.
    } else if (oldSize > newSize) {
      std::destroy(thisBegin + commonSize, thisBegin + oldSize);
    }

    // Destroy all of the elements in the other array.
    std::destroy(other.begin(), other.begin() + oldSize);
  }

  /// Given thait this array contains exactly oldSize initialized elements,
  /// destroy those elements, leaving it with no initialized elements.
  void destroy(size_t oldSize) {
    assert(oldSize <= N);
    std::destroy(begin(), begin() + oldSize);
  }
};

} // end namespace swift

#endif
