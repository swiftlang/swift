//===--- RefCount.cpp -----------------------------------------------------===//
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

#include <cstdio>

#include "swift/Runtime/HeapObject.h"

namespace swift {

// Return an object's side table, allocating it if necessary.
// Returns null if the object is deiniting.
// SideTableRefCountBits specialization intentionally does not exist.
template <>
HeapObjectSideTableEntry* RefCounts<InlineRefCountBits>::allocateSideTable(bool failIfDeiniting)
{
  auto oldbits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);

  // Preflight failures before allocating a new side table.
  if (oldbits.hasSideTable()) {
    // Already have a side table. Return it.
    return oldbits.getSideTable();
  }
  else if (failIfDeiniting && oldbits.getIsDeiniting()) {
    // Already past the start of deinit. Do nothing.
    return nullptr;
  }

  // Preflight passed. Allocate a side table.

  // FIXME: custom side table allocator
  auto side = swift_cxx_newObject<HeapObjectSideTableEntry>(getHeapObject());

  auto newbits = InlineRefCountBits(side);

  do {
    if (oldbits.hasSideTable()) {
      // Already have a side table. Return it and delete ours.
      // Read before delete to streamline barriers.
      auto result = oldbits.getSideTable();
      swift_cxx_deleteObject(side);
      return result;
    }
    else if (failIfDeiniting && oldbits.getIsDeiniting()) {
      // Already past the start of deinit. Do nothing.
      return nullptr;
    }

    side->initRefCounts(oldbits);

  } while (! refCounts.compare_exchange_weak(oldbits, newbits,
                                             std::memory_order_release,
                                             std::memory_order_relaxed));

  return side;
}


template <>
HeapObject *RefCounts<InlineRefCountBits>::incrementSlow(InlineRefCountBits oldbits,
                                                   uint32_t n) {
  if (oldbits.isImmortal(false)) {
    return getHeapObject();
  }
  else if (oldbits.hasSideTable()) {
    // Out-of-line slow path.
    auto side = oldbits.getSideTable();
    side->incrementStrong(n);
  }
  else {
    // Overflow into a new side table.
    auto side = allocateSideTable(false);
    side->incrementStrong(n);
  }
  return getHeapObject();
}
template <>
HeapObject *RefCounts<SideTableRefCountBits>::incrementSlow(SideTableRefCountBits oldbits,
                                                uint32_t n) {
  if (oldbits.isImmortal(false)) {
    return getHeapObject();
  }
  else {
    // Retain count overflow.
    swift::swift_abortRetainOverflow();
  }
  return getHeapObject();
}

template <>
void RefCounts<InlineRefCountBits>::incrementNonAtomicSlow(InlineRefCountBits oldbits,
                                                     uint32_t n) {
  if (oldbits.isImmortal(false)) {
    return;
  }
  else if (oldbits.hasSideTable()) {
    // Out-of-line slow path.
    auto side = oldbits.getSideTable();
    side->incrementStrong(n);  // FIXME: can there be a nonatomic impl?
  } else {
    // Overflow into a new side table.
    auto side = allocateSideTable(false);
    side->incrementStrong(n);  // FIXME: can there be a nonatomic impl?
  }
}
template <>
void RefCounts<SideTableRefCountBits>::incrementNonAtomicSlow(SideTableRefCountBits oldbits, uint32_t n) {
  if (oldbits.isImmortal(false)) {
    return;
  } else {
    swift::swift_abortRetainOverflow();
  }
}

template <typename RefCountBits>
bool RefCounts<RefCountBits>::tryIncrementSlow(RefCountBits oldbits) {
  if (oldbits.isImmortal(false)) {
    return true;
  }
  else if (oldbits.hasSideTable())
    return oldbits.getSideTable()->tryIncrement();
  else
    swift::swift_abortRetainOverflow();
}
template bool RefCounts<InlineRefCountBits>::tryIncrementSlow(InlineRefCountBits oldbits);
template bool RefCounts<SideTableRefCountBits>::tryIncrementSlow(SideTableRefCountBits oldbits);

template <typename RefCountBits>
bool RefCounts<RefCountBits>::tryIncrementNonAtomicSlow(RefCountBits oldbits) {
  if (oldbits.isImmortal(false)) {
    return true;
  }
  else if (oldbits.hasSideTable())
    return oldbits.getSideTable()->tryIncrementNonAtomic();
  else
    swift::swift_abortRetainOverflow();
}
template bool RefCounts<InlineRefCountBits>::tryIncrementNonAtomicSlow(InlineRefCountBits oldbits);
template bool RefCounts<SideTableRefCountBits>::tryIncrementNonAtomicSlow(SideTableRefCountBits oldbits);

// SideTableRefCountBits specialization intentionally does not exist.
template <>
HeapObjectSideTableEntry* RefCounts<InlineRefCountBits>::formWeakReference()
{
  auto side = allocateSideTable(true);
  if (side)
    return side->incrementWeak();
  else
    return nullptr;
}

template <typename RefCountBits>
void RefCounts<RefCountBits>::incrementUnownedSlow(uint32_t n) {
  auto side = allocateSideTable(false);
  if (side)
    return side->incrementUnowned(n);
  // Overflow but side table allocation failed.
  swift_abortUnownedRetainOverflow();
}

template void RefCounts<InlineRefCountBits>::incrementUnownedSlow(uint32_t n);
template <>
void RefCounts<SideTableRefCountBits>::incrementUnownedSlow(uint32_t n) {
  // Overflow from side table to a new side table?!
  swift_abortUnownedRetainOverflow();
}
  
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_API
void _swift_stdlib_immortalize(void *obj) {
  auto heapObj = reinterpret_cast<HeapObject *>(obj);
  heapObj->refCounts.setIsImmortal(true);
}

#ifndef NDEBUG
// SideTableRefCountBits specialization intentionally does not exist.
template <>
bool RefCounts<InlineRefCountBits>::isImmutableCOWBuffer() {
  if (!hasSideTable())
    return false;
  HeapObjectSideTableEntry *sideTable = allocateSideTable(false);
  assert(sideTable);
  return sideTable->isImmutableCOWBuffer();
}

template <>
bool RefCounts<InlineRefCountBits>::setIsImmutableCOWBuffer(bool immutable) {
  HeapObjectSideTableEntry *sideTable = allocateSideTable(false);
  assert(sideTable);
  bool oldValue = sideTable->isImmutableCOWBuffer();
  sideTable->setIsImmutableCOWBuffer(immutable);
  return oldValue;
}

#endif

template <typename RefCountBits>
void RefCounts<RefCountBits>::dump() const {
  printf("Location: %p\n", this);
  printf("Strong Ref Count: %d.\n", getCount());
  printf("Unowned Ref Count: %d.\n", getUnownedCount());
  printf("Weak Ref Count: %d.\n", getWeakCount());
  printf("RefCount Side Table: %p.\n", getSideTable());
  printf("Is Deiniting: %s.\n", isDeiniting() ? "true" : "false");
  printf("Is Immortal: %s.\n", refCounts.load().isImmortal(false) ? "true" : "false");
}

// namespace swift
} // namespace swift


