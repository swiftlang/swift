//===--- RefCount.cpp -----------------------------------------------------===//
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

#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Errors.h"

#define relaxed std::memory_order_relaxed
#define acquire std::memory_order_acquire
#define release std::memory_order_release
#define consume std::memory_order_consume


namespace swift {

template <typename RefCountBits>
void RefCounts<RefCountBits>::incrementSlow(RefCountBits oldbits,
                                            uint32_t n) {
  if (oldbits.hasSideTable()) {
    // Out-of-line slow path.
    auto side = oldbits.getSideTable();
    side->incrementStrong(n);
  }
  else {
    // Retain count overflow.
    swift::swift_abortRetainOverflow();
  }
}
template void RefCounts<InlineRefCountBits>::incrementSlow(InlineRefCountBits oldbits, uint32_t n);
template void RefCounts<SideTableRefCountBits>::incrementSlow(SideTableRefCountBits oldbits, uint32_t n);

template <typename RefCountBits>
void RefCounts<RefCountBits>::incrementNonAtomicSlow(RefCountBits oldbits,
                                                     uint32_t n) {
  if (oldbits.hasSideTable()) {
    // Out-of-line slow path.
    auto side = oldbits.getSideTable();
    side->incrementStrong(n);  // FIXME: can there be a nonatomic impl?
  } else {
    swift::swift_abortRetainOverflow();
  }
}
template void RefCounts<InlineRefCountBits>::incrementNonAtomicSlow(InlineRefCountBits oldbits, uint32_t n);
template void RefCounts<SideTableRefCountBits>::incrementNonAtomicSlow(SideTableRefCountBits oldbits, uint32_t n);

template <typename RefCountBits>
bool RefCounts<RefCountBits>::tryIncrementAndPinSlow() {
  abort();
}
template bool RefCounts<InlineRefCountBits>::tryIncrementAndPinSlow();
template bool RefCounts<SideTableRefCountBits>::tryIncrementAndPinSlow();

template <typename RefCountBits>
bool RefCounts<RefCountBits>::tryIncrementAndPinNonAtomicSlow() {
  abort();
}
template bool RefCounts<InlineRefCountBits>::tryIncrementAndPinNonAtomicSlow();
template bool RefCounts<SideTableRefCountBits>::tryIncrementAndPinNonAtomicSlow();


// SideTableRefCountBits specialization intentionall does not exist.
template <typename RefCountBits>
bool RefCounts<RefCountBits>::tryIncrementSlow(RefCountBits oldbits) {
  if (oldbits.hasSideTable())
    return oldbits.getSideTable()->tryIncrement();
  else
    swift::swift_abortRetainOverflow();
}
template bool RefCounts<InlineRefCountBits>::tryIncrementSlow(InlineRefCountBits oldbits);
template bool RefCounts<SideTableRefCountBits>::tryIncrementSlow(SideTableRefCountBits oldbits);

// SideTableRefCountBits specialization intentionally does not exist.
template <>
HeapObjectSideTableEntry* RefCounts<InlineRefCountBits>::allocateSideTable()
{
  HeapObjectSideTableEntry *side = new HeapObjectSideTableEntry(getHeapObject());
  
  auto newbits = InlineRefCountBits(side);
  
  auto oldbits = refCounts.load(relaxed);
  
  do {
    if (oldbits.hasSideTable()) {
      // Read before delete to streamline barriers.
      auto result = oldbits.getSideTable();
      delete side;
      return result;
    }
    
    // FIXME: assert not deiniting or something?
    
    // FIXME: barriers?
    side->initRefCounts(oldbits);
    
  } while (! refCounts.compare_exchange_weak(oldbits, newbits,
                                             release, relaxed));
  return side;
}



// SideTableRefCountBits specialization intentionally does not exist.
template <>
HeapObjectSideTableEntry* RefCounts<InlineRefCountBits>::formWeakReference()
{
  auto bits = refCounts.load(relaxed);
  if (!bits.hasSideTable() && bits.getIsDeiniting()) {
    // Already past the start of deinit. Do nothing.
    return nil;
  }
  
  auto side = allocateSideTable();
  return side->incrementWeak();
}

// namespace swift
}


