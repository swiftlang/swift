//===--- ImmutablePointerSet.h ----------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This file contains an implementation of a bump ptr allocated immutable
/// pointer set.
///
/// The target of this data structure are sets of pointers (with N < 100) that
/// are propagated through many basic blocks. These pointer sets will be merged
/// and copied far more than being created from an array.
///
/// Thus we assume the following constraints:
///
/// 1. Our set operations are purely additive. Given a set, one can only add
/// elements to it. One can not remove elements to it. This means we only
/// support construction of sets from arrays and concatenation of pointer sets.
///
/// 2. Our sets must always be ordered and be able to be iterated over
/// efficiently in that order.
///
/// 3. An O(log(n)) set contains method.
///
/// Beyond these constraints, we would like for our data structure to have the
/// following properties for performance reasons:
///
/// 1. Its memory should be bump ptr allocated. We like fast allocation.
///
/// 2. No destructors need to be called when the bump ptr allocator is being
/// destroyed. We like fast destruction and do not want to have to iterate over
/// potentially many of these sets and invoke destructors.
///
/// Thus our design is to represent our sets as bump ptr allocated arrays whose
/// elements are sorted and uniqued. The actual uniquing of the arrays
/// themselves is performed via folding set node.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_IMMUTABLEPOINTERSET_H
#define SWIFT_BASIC_IMMUTABLEPOINTERSET_H

#include "swift/Basic/STLExtras.h"
#include "swift/Basic/NullablePtr.h"
#include "llvm/Support/Allocator.h"
#include "llvm/ADT/FoldingSet.h"
#include <algorithm>
#include <type_traits>

// Include these here for now to reduce build times while I am testing. Putting
// things into STLExtras causes pretty much everything to recompile.
namespace swift {

/// @{

/// The equivalent of std::for_each, but visits the set union of two sorted
/// lists without allocating additional memory.
template <typename InputIt1, typename InputIt2, typename BinaryFunction>
inline void set_union_for_each(InputIt1 I1, InputIt1 E1, InputIt2 I2,
                               InputIt2 E2, BinaryFunction f) {
  while (true) {
    // If we have reached the end of either list, visit the rest of the other
    // list, We do not need to worry about duplicates since each array we know
    // is unique.
    if (I1 == E1) {
      std::for_each(I2, E2, f);
      return;
    }

    if (I2 == E2) {
      std::for_each(I1, E1, f);
      return;
    }

    // If I1 < I2, then visit I1 and continue.
    if (*I1 < *I2) {
      f(*I1);
      ++I1;
      continue;
    }

    // If I2 < I1, visit I2 and continue.
    if (*I2 < *I1) {
      f(*I2);
      ++I2;
      continue;
    }

    // Otherwise, we know that I1 and I2 equal. We know that we can only have
    // one of each element in each list, so we can just visit I1 and continue.
    f(*I1);
    ++I1;
    ++I2;
  }
}

/// A container adapter for set_union_for_each.
template <typename Container1, typename Container2, typename BinaryFunction>
inline void set_union_for_each(Container1 &&C1, Container2 &&C2,
                               BinaryFunction f) {
  set_union_for_each(C1.begin(), C1.end(), C2.begin(), C2.end(), f);
}

/// @}

/// Returns true if [II, IE) is a sorted and uniqued array. Returns false
/// otherwise.
template <typename IterTy> bool is_sorted_and_uniqued(IterTy II, IterTy IE) {
  // The empty list is always sorted and uniqued.
  if (II == IE)
    return true;

  // The list of one element is always sorted and uniqued.
  auto LastI = II;
  ++II;
  if (II == IE)
    return true;

  // Otherwise, until we reach the end of the list...
  while (II != IE) {
    // If LastI is greater than II then we know that our array is not sorted. If
    // LastI equals II, then we know that our array is not unique. If both of
    // those are conditions are false, then visit the next iterator element.
    if (*LastI < *II) {
      LastI = II;
      ++II;
      continue;
    }

    // Return false otherwise.
    return false;
  }

  // Success!
  return true;
}

template <typename Container> bool is_sorted_and_uniqued(Container &&C) {
  return is_sorted_and_uniqued(C.begin(), C.end());
}

} // end swift namespace

namespace swift {

template <typename PtrTy> class ImmutablePointerSetFactory;

/// An immutable set of pointers. It is backed by a tail allocated sorted array
/// ref.
template <typename T> class ImmutablePointerSet : public llvm::FoldingSetNode {
  using PtrTy = typename std::add_pointer<T>::type;
  friend class ImmutablePointerSetFactory<T>;

  NullablePtr<ImmutablePointerSetFactory<T>> ParentFactory;
  ArrayRef<PtrTy> Data;

  ImmutablePointerSet(ImmutablePointerSetFactory<T> *ParentFactory,
                      ArrayRef<PtrTy> NewData)
      : ParentFactory(ParentFactory), Data(NewData) {}

public:
  ~ImmutablePointerSet() = default;
  ImmutablePointerSet(const ImmutablePointerSet &) = default;
  ImmutablePointerSet(ImmutablePointerSet &&) = default;

  ImmutablePointerSet &operator=(const ImmutablePointerSet &) = default;
  ImmutablePointerSet &operator=(ImmutablePointerSet &&) = default;

  bool operator==(const ImmutablePointerSet<T> &P) const {
    // If both are empty, they must be equivalent.
    if (empty() && P.empty())
      return true;
    // Ok, at least one is non-empty. If either are empty at this point, then we
    // are comparing a non-empty set with an empty set, i.e. they do not equal.
    if (empty() || P.empty())
      return false;

    // Ok, both sets are not empty. Compare their profiles.
    llvm::FoldingSetNodeID ID1, ID2;
    Profile(ID1);
    P.Profile(ID2);
    return ID1 == ID2;
  }

  bool operator!=(const ImmutablePointerSet<T> &P) const {
    return !(*this == P);
  }

  unsigned count(PtrTy Ptr) const {
    // This returns the first element >= Ptr. Since we know that our array is
    // sorted and uniqued, Ptr must be that element.
    auto LowerBound = std::lower_bound(begin(), end(), Ptr);

    // If Ptr is > than everything in the array, then we obviously have 0.
    if (LowerBound == end())
      return 0;

    // Then check if Ptr is > or Ptr is ==. We only have Ptr if we have == to.
    return *LowerBound == Ptr;
  }

  using iterator = typename decltype(Data)::iterator;
  iterator begin() const { return Data.begin(); }
  iterator end() const { return Data.end(); }

  unsigned size() const { return Data.size(); }
  bool empty() const { return Data.empty(); }

  void Profile(llvm::FoldingSetNodeID &ID) const {
    assert(!Data.empty() && "Should not profile empty ImmutablePointerSet");
    for (PtrTy P : Data) {
      ID.AddPointer(P);
    }
  }

  ImmutablePointerSet<T> *concat(ImmutablePointerSet<T> *Other) {
    if (empty())
      return Other;
    if (Other->empty())
      return this;
    assert(Other->ParentFactory.get() == ParentFactory.get());
    return ParentFactory.get()->concat(this, Other);
  }
};

template <typename T> class ImmutablePointerSetFactory {
  using PtrTy = typename std::add_pointer<T>::type;

  using PtrSet = ImmutablePointerSet<T>;
  static constexpr unsigned AllocAlignment =
      (alignof(PtrSet) > alignof(PtrTy)) ? alignof(PtrSet) : alignof(PtrTy);

  llvm::BumpPtrAllocator &Allocator;
  llvm::FoldingSetVector<PtrSet> Set;
  static PtrSet EmptyPtrSet;

public:
  ImmutablePointerSetFactory(llvm::BumpPtrAllocator &A) : Allocator(A), Set() {}
  ImmutablePointerSetFactory(const ImmutablePointerSetFactory &) = delete;
  ImmutablePointerSetFactory(ImmutablePointerSetFactory &&) = delete;
  ImmutablePointerSetFactory &
  operator=(const ImmutablePointerSetFactory &) = delete;
  ImmutablePointerSetFactory &operator=(ImmutablePointerSetFactory &&) = delete;

  // We use a sentinel value here so that we can create an empty value
  // statically.
  static PtrSet *getEmptySet() { return &EmptyPtrSet; }

  /// Given a sorted and uniqued list \p Array, return the ImmutablePointerSet
  /// containing Array. Asserts if \p Array is not sorted and uniqued.
  PtrSet *get(ArrayRef<PtrTy> Array) {
    if (Array.empty())
      return ImmutablePointerSetFactory::getEmptySet();

    // We expect our users to sort/unique the input array. This is because doing
    // it here would either require us to allocate more memory than we need or
    // write into the input Array, which we don't want.
    assert(is_sorted_and_uniqued(Array));

    llvm::FoldingSetNodeID ID;
    for (PtrTy Ptr : Array) {
      ID.AddPointer(Ptr);
    }

    void *InsertPt;
    if (auto *PSet = Set.FindNodeOrInsertPos(ID, InsertPt)) {
      return PSet;
    }

    size_t NumElts = Array.size();
    size_t MemSize = sizeof(PtrSet) + sizeof(PtrTy) * NumElts;

    // Allocate the memory.
    auto *Mem =
        reinterpret_cast<PtrSet *>(Allocator.Allocate(MemSize, AllocAlignment));

    // Copy in the pointers into the tail allocated memory. We do not need to do
    // any sorting/uniquing ourselves since we assume that our users perform
    // this task for us.
    MutableArrayRef<PtrTy> DataMem(reinterpret_cast<PtrTy *>(&Mem[1]), NumElts);
    std::copy(Array.begin(), Array.end(), DataMem.begin());

    // Allocate the new node and insert it into the Set.
    auto *NewNode = new (Mem) PtrSet(this, DataMem);
    Set.InsertNode(NewNode, InsertPt);
    return NewNode;
  }

  PtrSet *concat(PtrSet *S1, ArrayRef<PtrTy> S2) {
    if (S1->empty())
      return get(S2);

    if (S2.empty())
      return S1;

    // We assume that S2 is sorted and uniqued.
    assert(is_sorted_and_uniqued(S2));

    llvm::FoldingSetNodeID ID;

    // We know that both of our pointer sets are sorted, so we can essentially
    // perform a sorted set merging algorithm to create the ID. We also count
    // the number of unique elements for allocation purposes.
    unsigned NumElts = 0;
    set_union_for_each(*S1, S2, [&ID, &NumElts](const PtrTy Ptr) -> void {
      ID.AddPointer(Ptr);
      NumElts++;
    });

    // If we find our ID then continue.
    void *InsertPt;
    if (auto *PSet = Set.FindNodeOrInsertPos(ID, InsertPt)) {
      return PSet;
    }

    unsigned MemSize = sizeof(PtrSet) + sizeof(PtrTy) * NumElts;

    // Allocate the memory.
    auto *Mem =
        reinterpret_cast<PtrSet *>(Allocator.Allocate(MemSize, AllocAlignment));

    // Copy in the union of the two pointer sets into the tail allocated
    // memory. Since we know that our sorted arrays are uniqued, we can use
    // set_union to get the uniqued sorted array that we want.
    MutableArrayRef<PtrTy> DataMem(reinterpret_cast<PtrTy *>(&Mem[1]), NumElts);
    std::set_union(S1->begin(), S1->end(), S2.begin(), S2.end(),
                   DataMem.begin());

    // Allocate the new node, insert it into the Set, and return it.
    auto *NewNode = new (Mem) PtrSet(this, DataMem);
    Set.InsertNode(NewNode, InsertPt);
    return NewNode;
  }

  PtrSet *concat(PtrSet *S1, PtrSet *S2) {
    // If either S1 or S2 are the empty PtrSet, just return S2 or S1.
    if (S1->empty())
      return S2;
    if (S2->empty())
      return S1;

    llvm::FoldingSetNodeID ID;

    // We know that both of our pointer sets are sorted, so we can essentially
    // perform a sorted set merging algorithm to create the ID. We also count
    // the number of unique elements for allocation purposes.
    unsigned NumElts = 0;
    set_union_for_each(*S1, *S2, [&ID, &NumElts](const PtrTy Ptr) -> void {
      ID.AddPointer(Ptr);
      NumElts++;
    });

    // If we find our ID then continue.
    void *InsertPt;
    if (auto *PSet = Set.FindNodeOrInsertPos(ID, InsertPt)) {
      return PSet;
    }

    unsigned MemSize = sizeof(PtrSet) + sizeof(PtrTy) * NumElts;

    // Allocate the memory.
    auto *Mem =
        reinterpret_cast<PtrSet *>(Allocator.Allocate(MemSize, AllocAlignment));

    // Copy in the union of the two pointer sets into the tail allocated
    // memory. Since we know that our sorted arrays are uniqued, we can use
    // set_union to get the uniqued sorted array that we want.
    MutableArrayRef<PtrTy> DataMem(reinterpret_cast<PtrTy *>(&Mem[1]), NumElts);
    std::set_union(S1->begin(), S1->end(), S2->begin(), S2->end(),
                   DataMem.begin());

    // Allocate the new node, insert it into the Set, and return it.
    auto *NewNode = new (Mem) PtrSet(this, DataMem);
    Set.InsertNode(NewNode, InsertPt);
    return NewNode;
  }
};

template <typename T>
ImmutablePointerSet<T> ImmutablePointerSetFactory<T>::EmptyPtrSet =
    ImmutablePointerSet<T>(nullptr, {});

} // end swift namespace

#endif
