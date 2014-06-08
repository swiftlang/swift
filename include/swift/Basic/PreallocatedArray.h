//===- PreallocatedArray.h - Fixed size array -------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_PREALLOCATEDARRAY_H_
#define SWIFT_BASIC_PREALLOCATEDARRAY_H_

#include "llvm/Support/Allocator.h"
#include <type_traits>

namespace swift {

/// An array meant for large collections of default constructible elements of
/// fixed size. It does one large allocation on construction.
template <typename EltTy, typename AllocatorTy=llvm::MallocAllocator>
class PreallocatedArray {
  static_assert(std::is_default_constructible<EltTy>::value,
                "EltTy must be default constructable.");
  unsigned NumElts;
  Optional<AllocatorTy> Allocator;
  EltTy *Elt;

public:

  PreallocatedArray(unsigned NumElts)
    : NumElts(NumElts),
      Allocator(llvm::MallocAllocator()),
      Elt(new (Allocator->template Allocate<EltTy>(NumElts)) EltTy[NumElts]) {}

  PreallocatedArray(unsigned NumElts, AllocatorTy &Allocator)
    : NumElts(NumElts),
      Elt(new (Allocator.template Allocate<EltTy>(NumElts)) EltTy[NumElts]) {}

  // Call destructors of data.
  ~PreallocatedArray() {
    for (unsigned i = 0; i < NumElts; ++i) {
      Elt[i].~EltTy();
    }
  }

  MutableArrayRef<EltTy> getArray() {
    return MutableArrayRef<EltTy>(Elt, NumElts);
  }

  ArrayRef<EltTy> getArray() const {
    return ArrayRef<EltTy>(Elt, NumElts);
  }

  using iterator = typename MutableArrayRef<EltTy>::iterator;
  using const_iterator = typename ArrayRef<EltTy>::iterator;

  iterator begin() { return getArray().begin(); }
  iterator end() { return getArray().end(); }

  const_iterator begin() const { return getArray().begin(); }
  const_iterator end() const { return getArray().end(); }

  Range<iterator> getRange() { return {begin(), end()}; }
  Range<const_iterator> getRange() const { return {begin(), end()}; }  

  unsigned size() const { return NumElts; }
  EltTy &operator[](size_t Index) {
    return getArray()[Index];
  }
};

} // end namespace swift

#endif
