//===--- BlotSetVector.h ----------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_BLOTSETVECTOR_H
#define SWIFT_BASIC_BLOTSETVECTOR_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include <vector>

namespace swift {

/// This is a set container with the following properties:
///
/// 1. Fast insertion-order iteration.
///
/// 2. Stable index offsets for all values after all operations including
/// erase.
///
/// This contrasts with SetVector where index offsets are not stable due to
/// usage of std::vector::erase(). In contrast, BlotSetVector uses the `blot
/// operation' (a) which trades memory for runtime and index offset stability.
///
/// 3. Fast replacement of a value v1 with a second value v2 guaranteeing that
/// v2 is placed into the same array index as v1, just deleting v1 if v2 is
/// already in the array.
///
/// This is important if one has other data structures referring to v1 via v1's
/// index in the vector that one wishes to now refer to v2.
///
/// 4. Fast deletion via the 'blot' operation.
///
/// 5. Fast insertion.
///
/// (a) The `blot operation' is leaving the value in the set vector, but marking
/// the value as being dead.
template <typename ValueT, typename VectorT = std::vector<Optional<ValueT>>,
          typename MapT = llvm::DenseMap<ValueT, unsigned>>
class BlotSetVector {
  VectorT Vector;
  MapT Map;

public:
  /// Construct an empty BlotSetVector.
  BlotSetVector() {}

  bool empty() const { return Vector.empty(); }

  unsigned size() const { return Vector.size(); }

  using iterator = typename VectorT::iterator;
  using const_iterator = typename VectorT::const_iterator;
  iterator begin() { return Vector.begin(); }
  iterator end() { return Vector.end(); }
  const_iterator begin() const { return Vector.begin(); }
  const_iterator end() const { return Vector.end(); }
  llvm::iterator_range<const_iterator> getRange() const {
    return {begin(), end()};
  }

  using const_reverse_iterator = typename VectorT::const_reverse_iterator;
  const_reverse_iterator rbegin() const { return Vector.rbegin(); }
  const_reverse_iterator rend() const { return Vector.rend(); }
  llvm::iterator_range<const_reverse_iterator> getReverseRange() const {
    return {rbegin(), rend()};
  }

  const Optional<ValueT> &operator[](unsigned n) const {
    assert(n < Vector.size() && "Out of range!");
    return Vector[n];
  }

  /// Insert \p V into the SetVector if it is not in the array and return the
  /// index of \p V in the Set Vector. If \p V is already in the SetVector, just
  /// return its index in the array.
  unsigned insert(const ValueT &V) {
    auto Iter = Map.find(V);
    if (Iter != Map.end())
      return Iter->second;

    unsigned Index = Vector.size();
    Map[V] = Index;
    Vector.push_back(V);
    return Index;
  }

  /// Replace \p V1 with \p V2 placing \p V2 into the position in the array
  /// where V1 used to be. If \p V2 is already in the set, this just erases \p
  /// V1.
  void replace(const ValueT &V1, const ValueT &V2) {
    auto Iter1 = Map.find(V1);
    assert(Iter1 != Map.end() && "Cannot replace value that is not in set");
    unsigned V1Index = Iter1->second;
    Map.erase(V1);

    auto Iter2 = Map.find(V2);
    if (Iter2 != Map.end()) {
      Vector[V1Index] = None;
      return;
    }

    Map[V2] = V1Index;
    Vector[V1Index] = V2;
  }

  /// Erase the value \p V if it is in the set. Returns true if V was
  /// successfully erased and false otherwise.
  bool erase(const ValueT &V) {
    auto Iter = Map.find(V);
    if (Iter == Map.end())
      return false;
    unsigned Index = Iter->second;
    Map.erase(Iter);
    Vector[Index] = None;
    return true;
  }

  /// Return the last element of the blot map vector. Will be None if blotted.
  Optional<ValueT> pop_back_val() {
    auto result = Vector.pop_back_val();
    if (!result)
      return result;
    Map.erase(*result);
    return result;
  }

  /// Attempt to lookup the index of \p V. Returns None upon failure and the
  /// value on success.
  Optional<unsigned> getIndex(const ValueT &V) {
    auto Iter = Map.find(V);
    if (Iter == Map.end())
      return None;
    return Iter->second;
  }
};

template <typename ValueT, unsigned N,
          typename VectorT = llvm::SmallVector<Optional<ValueT>, N>,
          typename MapT = llvm::SmallDenseMap<ValueT, unsigned, N>>
class SmallBlotSetVector : public BlotSetVector<ValueT, VectorT, MapT> {
public:
  SmallBlotSetVector() {}
};

} // end swift namespace

#endif // SWIFT_BASIC_BLOTSETVECTOR_H
