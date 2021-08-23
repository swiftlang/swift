//===--- SmallPtrSetVector.h ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_SMALLPTRSETVECTOR_H
#define SWIFT_BASIC_SMALLPTRSETVECTOR_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

/// A SetVector that performs no allocations if smaller than a certain
/// size. Uses a SmallPtrSet/SmallVector internally.
template <typename T, unsigned VectorSize, unsigned SetSize = VectorSize>
class SmallPtrSetVector : public llvm::SetVector<T, SmallVector<T, VectorSize>,
                                                 SmallPtrSet<T, SetSize>> {
public:
  SmallPtrSetVector() = default;

  /// Initialize a SmallPtrSetVector with a range of elements
  template <typename It> SmallPtrSetVector(It Start, It End) {
    this->insert(Start, End);
  }
};

} // namespace swift

namespace std {

/// Implement std::swap in terms of SmallSetVector swap.
///
/// This matches llvm's implementation for SmallSetVector.
template <typename T, unsigned VectorSize, unsigned SetSize = VectorSize>
inline void swap(swift::SmallPtrSetVector<T, VectorSize, SetSize> &LHS,
                 swift::SmallPtrSetVector<T, VectorSize, SetSize> &RHS) {
  LHS.swap(RHS);
}

} // end namespace std

#endif
