//===- PreallocatedMap.h - Fixed size, sorted map ---------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_PREALLOCATEDMAP_H_
#define SWIFT_BASIC_PREALLOCATEDMAP_H_

#include <functional>
#include "swift/Basic/PreallocatedArray.h"

namespace swift {

/// A map with a fixed constant size. The way to use it is to first go through
/// as an array initializing all the data structures. Then sort call sort.
template <typename KeyTy, typename ValueTy,
          typename AllocatorTy=llvm::MallocAllocator>
class PreallocatedMap {
public:
  using PairTy = std::pair<KeyTy, ValueTy>;
  using SortFunTy = std::function<bool (const PairTy &, const PairTy &)>;

private:
  using ArrayTy = PreallocatedArray<PairTy, AllocatorTy>;

  bool IsSorted = false;
  ArrayTy Array;
  SortFunTy SortFun;

public:
  PreallocatedMap(unsigned NumElts, SortFunTy SortFun) : Array(NumElts),
                                                         SortFun(SortFun) {}

  PreallocatedMap(unsigned NumElts, AllocatorTy &Allocator, SortFunTy SortFun)
    : Array(NumElts, Allocator), SortFun(SortFun) {}

  // Make the map immutable by setting the immutable flag and sorting the
  // map. Only after this is called can one search for things in the map.
  void sort() {
    if (IsSorted)
      return;
    IsSorted = true;
    std::sort(Array.begin(), Array.end(), SortFun);
  }

  // Returns whether the array is sorted.
  bool isSorted() const { return IsSorted; }

  PairTy &operator[](size_t Index) { return Array[Index]; }
  ValueTy &operator[](KeyTy &Key) {
    auto P = find(Key);
    assert(P != end() && "Unknown key!");
    return P->second;
  }
  unsigned size() const { return Array.size(); }

  using iterator = typename ArrayTy::iterator;
  using const_iterator = typename ArrayTy::const_iterator;

  iterator begin() { return Array.begin(); }
  iterator end() { return Array.end(); }
  const_iterator begin() const { return Array.begin(); }
  const_iterator end() const { return Array.end(); }
  Range<iterator> getRange() { return Array.getRange(); }
  Range<const_iterator> getRange() const { return Array.getRange(); }

  iterator find(KeyTy &Key) {
    std::pair<KeyTy, ValueTy> Pair;
    Pair.first = Key;
    return std::lower_bound(Array.begin(), Array.end(), Pair, SortFun); 
  }
};

} // end namespace swift

#endif
