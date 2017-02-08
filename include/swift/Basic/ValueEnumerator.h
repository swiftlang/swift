//===--- ValueEnumerator.h --- Enumerates values ----------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_VALUEENUMERATOR_H
#define SWIFT_BASIC_VALUEENUMERATOR_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

/// / This class maps values to unique indices.
template<class ValueTy, class IndexTy = size_t>
class ValueEnumerator {
  /// A running counter to enumerate values.
  IndexTy counter = 0;

  /// Maps values to unique integers.
  llvm::DenseMap<ValueTy, IndexTy> ValueToIndex;

public:
  /// Return the index of value \p v.
  IndexTy getIndex(const ValueTy &v) {
    // Return the index of this Key, if we've assigned one already.
    auto It = ValueToIndex.find(v);
    if (It != ValueToIndex.end()) {
      return It->second;
    }

    // Generate a new counter for the key.
    ValueToIndex[v] = ++counter;
    return counter;
  }

  ValueEnumerator() = default;

  /// Forget about key \p v.
  void invalidateValue(const ValueTy &v) { ValueToIndex.erase(v); }

  /// Clear the enumeration state of the
  void clear() {
    ValueToIndex.clear();
    counter = 0;
  }
};

} // end namespace swift

#endif // SWIFT_BASIC_VALUEENUMERATOR_H
