//===--- StackList.h - defines the StackList data structure -----*- C++ -*-===//
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

#ifndef SWIFT_SIL_STACKLIST_H
#define SWIFT_SIL_STACKLIST_H

#include "swift/SIL/SILModule.h"

namespace swift {

/// A very efficient implementation of a stack, which can also be iterated over.
///
/// A StackList is the best choice for things like worklists, etc., if no random
/// access is needed.
/// Regardless of how large a Stack gets, there is no memory allocation needed
/// (except maybe for the first few uses in the compiler run).
/// All operations have (almost) zero cost.
template <typename Element> class StackList {
  /// The capacity of a single slab.
  static constexpr size_t slabCapacity =
      FixedSizeSlab::capacity / sizeof(Element);
  
  static_assert(slabCapacity > 0, "Element type to large for StackList");
  static_assert(alignof(FixedSizeSlab) >= alignof(Element),
                "Element alignment to large for StackList");
  
  /// Backlink to the module which manages the slab allocation.
  SILModule &module;

  /// The list of slabs.
  ///
  /// Invariant: there is always free space in the last slab to store at least
  /// one element.
  SILModule::SlabList slabs;
  
  /// The index of the next free element in endSlab.
  ///
  /// Invariant: endIndex < slabCapacity
  unsigned endIndex = 0;

  FixedSizeSlab *lastSlab() { return &*slabs.rbegin(); }
  const FixedSizeSlab *lastSlab() const { return &*slabs.rbegin(); }

  void allocSlab() { slabs.push_back(*module.allocSlab()); }

  void growIfNeeded() {
    if (endIndex == slabCapacity) {
      allocSlab();
      endIndex = 0;
    }
  }
  

public:
  /// The Stack's iterator.
  class iterator {
    friend StackList<Element>;
    const FixedSizeSlab *slab;
    unsigned index;

    iterator(const FixedSizeSlab *slab, unsigned index)
      : slab(slab), index(index) {}
      
  public:
    const Element &operator*() const {
      assert(index < slabCapacity);
      return slab->dataFor<Element>()[index];
    }
    const Element &operator->() const { return *this; }

    iterator &operator++() {
      assert(index < slabCapacity);
      index++;
      if (index == slabCapacity) {
        slab = &*std::next(slab->getIterator());
        index = 0;
      }
      return *this;
    }
    
    iterator operator++(int unused) {
      iterator copy = *this;
      ++*this;
      return copy;
    }

    friend bool operator==(iterator lhs, iterator rhs) {
      return lhs.slab == rhs.slab && lhs.index == rhs.index;
    }

    friend bool operator!=(iterator lhs, iterator rhs) {
      return !(lhs == rhs);
    }
  };

  /// Constructor.
  StackList(SILFunction *function) : module(function->getModule()) {
    /// Allocate one slab so that there is free space for inserting the first
    /// element.
    allocSlab();
  }

  ~StackList() {
    module.freeAllSlabs(slabs);
  }

  iterator begin() const { return iterator(&*slabs.begin(), 0); }
  iterator end() const { return iterator(lastSlab(), endIndex); }
  
  bool empty() const {
    return begin() == end();
  }

  /// Adds a new element at the end.
  void push_back(const Element &newElement) {
    assert(endIndex < slabCapacity);
    lastSlab()->template dataFor<Element>()[endIndex++] = newElement;
    growIfNeeded();
  }

  /// Adds a new element at the end.
  void push_back(Element &&newElement) {
    assert(endIndex < slabCapacity);
    lastSlab()->template dataFor<Element>()[endIndex++] = std::move(newElement);
    growIfNeeded();
  }

  /// Removes the last element and returns it.
  Element pop_back_val() {
    FixedSizeSlab *slab = lastSlab();
    if (endIndex > 0)
      return std::move(slab->dataFor<Element>()[--endIndex]);

    assert(!empty());
  
    slabs.remove(*slab);
    module.freeSlab(slab);
    assert(!slabs.empty());
    endIndex = slabCapacity - 1;
    return std::move(lastSlab()->template dataFor<Element>()[endIndex]);
  }
};

} // namespace swift

#endif
