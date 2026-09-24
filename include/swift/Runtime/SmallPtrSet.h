//===--- SmallPtrSet.h - A small, allocator-parameterized pointer set ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A pointer set with a small-buffer optimization, in the spirit of
// llvm::SmallPtrSet: up to SmallSize elements are kept inline (and found via
// linear scan), and the set is promoted to a heap-allocated open-addressed
// hash table only once it grows beyond that. Unlike llvm::SmallPtrSet, the
// heap allocation is routed through an STL-allocator-conformant type
// (defaulting to swift::cxx_allocator, which uses swift_slowAlloc /
// swift_slowDealloc) instead of malloc/free, so this is usable from runtime
// code that must work under -ffreestanding, where malloc/free and the C++
// standard library's own containers are unavailable.
//
// Only insertion is supported: that's all that's needed to deduplicate keys
// while walking a list, and it lets the hash table avoid tombstones.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_SMALLPTRSET_H
#define SWIFT_RUNTIME_SMALLPTRSET_H

#include <cstdint>
#include <cstring>
#include <type_traits>

#include "swift/Runtime/Heap.h"

namespace swift {
namespace runtime {

template <typename PtrType, unsigned SmallSize,
          typename Allocator = cxx_allocator<const void *>>
class SmallPtrSet {
  static_assert(std::is_pointer<PtrType>::value,
                "SmallPtrSet can only store pointers");
  static_assert(SmallSize > 0 && (SmallSize & (SmallSize - 1)) == 0,
                "SmallSize must be a power of two");

  static const void *getEmptyMarker() {
    return reinterpret_cast<const void *>(-1);
  }

  const void *SmallStorage[SmallSize];
  const void **CurArray = SmallStorage;
  unsigned CurArraySize = SmallSize;
  unsigned NumNonEmpty = 0;

  bool isSmall() const { return CurArray == SmallStorage; }

  static unsigned hashPointer(const void *ptr) {
    auto value = reinterpret_cast<uintptr_t>(ptr);
    return unsigned(value >> 4) ^ unsigned(value >> 9);
  }

  /// Finds the bucket that Ptr either already occupies, or should be
  /// inserted into. CurArraySize must be a power of two, and the array must
  /// not be completely full (there must be at least one empty bucket).
  const void **findBucketFor(const void *ptr) {
    unsigned bucket = hashPointer(ptr) & (CurArraySize - 1);
    unsigned probeAmt = 1;
    while (true) {
      const void *entry = CurArray[bucket];
      if (entry == getEmptyMarker() || entry == ptr)
        return CurArray + bucket;
      bucket = (bucket + probeAmt++) & (CurArraySize - 1);
    }
  }

  /// Allocate a new backing array of the given size (a power of two) and
  /// rehash the existing elements into it.
  void grow(unsigned newSize) {
    const void **oldArray = CurArray;
    const void **oldEnd = isSmall() ? CurArray + NumNonEmpty
                                     : CurArray + CurArraySize;
    unsigned oldArraySize = CurArraySize;
    bool wasSmall = isSmall();

    Allocator alloc;
    CurArray = alloc.allocate(newSize);
    CurArraySize = newSize;
    std::memset(CurArray, -1, newSize * sizeof(void *));

    for (const void **p = oldArray; p != oldEnd; ++p)
      *findBucketFor(*p) = *p;

    if (!wasSmall)
      alloc.deallocate(oldArray, oldArraySize);
  }

  bool insertBig(const void *ptr) {
    if ((NumNonEmpty + 1) * 4 >= CurArraySize * 3)
      grow(CurArraySize < 64 ? 128 : CurArraySize * 2);

    const void **bucket = findBucketFor(ptr);
    if (*bucket == ptr)
      return false;
    *bucket = ptr;
    ++NumNonEmpty;
    return true;
  }

public:
  SmallPtrSet() = default;
  SmallPtrSet(const SmallPtrSet &) = delete;
  SmallPtrSet &operator=(const SmallPtrSet &) = delete;

  ~SmallPtrSet() {
    if (!isSmall())
      Allocator().deallocate(CurArray, CurArraySize);
  }

  /// Inserts Ptr into the set. Returns true if it was newly inserted, false
  /// if it was already present.
  bool insert(PtrType Ptr) {
    const void *VP = reinterpret_cast<const void *>(Ptr);
    if (isSmall()) {
      for (unsigned i = 0; i != NumNonEmpty; ++i)
        if (CurArray[i] == VP)
          return false;
      if (NumNonEmpty < CurArraySize) {
        CurArray[NumNonEmpty++] = VP;
        return true;
      }
    }
    return insertBig(VP);
  }
};

} // namespace runtime
} // namespace swift

#endif // SWIFT_RUNTIME_SMALLPTRSET_H
