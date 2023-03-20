//===--- Generators.h - "Coroutines" for doing traversals -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines a basic generator concept and some useful common
// implementations of it.
//
// concept Generator {
//   // ...some number of accessors for the current value...
//
//   /// Is this generator finished producing values?
//   bool isFinished() const;
//
//   /// Given that this generator is not finished, advance to the
//   /// next value.
//   void advance();
//
//   /// Finish the generator, asserting that all values have been
//   /// produced.
//   void finish();
// };
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_GENERATORS_H
#define SWIFT_BASIC_GENERATORS_H

#include "llvm/ADT/ArrayRef.h"

namespace swift {

namespace generator_details {

template <class T>
struct is_array_ref_like {
  enum { value = false };
};

template <class T>
struct is_array_ref_like<llvm::ArrayRef<T>> {
  enum { value = true };
};

template <class T>
struct is_array_ref_like<llvm::MutableArrayRef<T>> {
  enum { value = true };
};
}

/// A class for generating the elements of an ArrayRef-like collection.
template <class CollectionType>
class ArrayRefGenerator {
  static_assert(generator_details::is_array_ref_like<CollectionType>::value,
                "ArrayRefGenerator should only be used with ArrayRef-like "
                "types");

  CollectionType values;

public:
  using reference =
    typename std::iterator_traits<typename CollectionType::iterator>::reference;

  ArrayRefGenerator() {}
  ArrayRefGenerator(CollectionType values) : values(values) {}

  // Prevent accidental copying of the generator.
  ArrayRefGenerator(const ArrayRefGenerator &other) = delete;
  ArrayRefGenerator &operator=(const ArrayRefGenerator &other) = delete;

  ArrayRefGenerator(ArrayRefGenerator &&other) = default;
  ArrayRefGenerator &operator=(ArrayRefGenerator &&other) = default;

  /// Explicitly copy the current generator state.
  ArrayRefGenerator clone() const {
    return ArrayRefGenerator(values);
  }

  /// Return the current element of the array.
  reference getCurrent() const {
    assert(!isFinished());
    return values.front();
  }

  /// Claim the current element of the array and advance past it.
  reference claimNext() {
    assert(!isFinished());
    reference result = getCurrent();
    advance();
    return result;
  }

  /// Claim the next N elements of the array and advance past them.
  CollectionType claimNext(size_t count) {
    assert(count <= values.size() && "claiming too many values");
    CollectionType result = values.slice(0, count);
    values = values.slice(count);
    return result;
  }

  /// Is this generation finished?
  bool isFinished() const {
    return values.empty();
  }

  /// Given that this generation is not finished, advance to the
  /// next element.
  void advance() {
    assert(!isFinished());
    values = values.slice(1);
  }

  /// Perform any final work required to complete the generation.
  void finish() {
    assert(isFinished() && "didn't finish generating the collection");
  }
};

} // end namespace swift

#endif
