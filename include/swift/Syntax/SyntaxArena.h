//===--- SyntaxArena.h - Syntax Tree Memory Allocation ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines SyntaxArena that is Memory manager for Syntax nodes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_SYNTAXARENA_H
#define SWIFT_SYNTAX_SYNTAXARENA_H

#include "swift/Syntax/References.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/Support/Allocator.h"

namespace swift {
namespace syntax {

/// Memory manager for Syntax nodes.
class SyntaxArena {
  friend class WithExcluseiveSyntaxArenaAccessRAII;

  SyntaxArena(const SyntaxArena &) = delete;
  void operator=(const SyntaxArena &) = delete;

  llvm::BumpPtrAllocator Allocator;

  /// The start (inclusive) and end (exclusive) pointers of a memory region that
  /// is frequently requested using \c containsPointer. Must be inside \c
  /// Allocator but \c containsPointer will check this region first and will
  /// thus return more quickly for pointers that lie within this region.
  const void *HotUseMemoryRegionStart = nullptr;
  const void *HotUseMemoryRegionEnd = nullptr;

  mutable volatile int RefCount = 0;

  /// While \c HasExclusiveAccess is true, only a single thread accesses this
  /// \c SyntaxArena and the \c RawSyntax nodes contained within. Thus, all
  /// reference counting does not have to be thread-safe, which can
  /// significantly improve performance.
  bool HasExclusiveAccess = false;

public:
  SyntaxArena() {}

  static RC<SyntaxArena> make() { return RC<SyntaxArena>(new SyntaxArena()); }

  void Retain() const {
    if (HasExclusiveAccess) {
      ++RefCount;
    } else {
      // The storage of std::atomic<int> is simply an int, so we can
      // reinterpret our unsafe ref-count as atomic<int> to perform an atomic
      // operation on it.
      __atomic_fetch_add(&RefCount, 1, std::memory_order_relaxed);
    }
  }

  void Release() const {
    int NewRefCount;
    if (HasExclusiveAccess) {
      NewRefCount = --RefCount;
    } else {
      // See comment in SyntaxArena::Retain.
      NewRefCount =
          __atomic_fetch_sub(&RefCount, 1, std::memory_order_relaxed) - 1;
    }
    assert(NewRefCount >= 0 && "Reference count was already zero.");
    if (NewRefCount == 0) {
      delete this;
    }
  }

  bool hasExclusiveAccess() const { return HasExclusiveAccess; }

  void setHotUseMemoryRegion(const void *Start, const void *End) {
    assert(containsPointer(Start) &&
           "The hot use memory region should be in the Arena's bump allocator");
    HotUseMemoryRegionStart = Start;
    HotUseMemoryRegionEnd = End;
  }

  llvm::BumpPtrAllocator &getAllocator() { return Allocator; }
  void *Allocate(size_t size, size_t alignment) {
    return Allocator.Allocate(size, alignment);
  }

  bool containsPointer(const void *Ptr) {
    if (HotUseMemoryRegionStart <= Ptr && Ptr < HotUseMemoryRegionEnd) {
      return true;
    }
    return getAllocator().identifyObject(Ptr) != llvm::None;
  }
};

/// Having this RAII alive gives the current thread exclusive access to the
/// passed \c SyntaxArena and all \c RawSyntax nodes stored within. Accessing
/// any of the nodes from a different thread while the RAII is alive might lead
/// to undefined behaviour regarding the nodes' reference counting.
class WithExcluseiveSyntaxArenaAccessRAII {
  RC<SyntaxArena> Arena;

public:
  WithExcluseiveSyntaxArenaAccessRAII(const RC<SyntaxArena> &Arena)
      : Arena(Arena) {
    assert(!Arena->HasExclusiveAccess && "Arena already has exclusive access?");
    Arena->HasExclusiveAccess = true;
  }

  ~WithExcluseiveSyntaxArenaAccessRAII() {
    assert(Arena->HasExclusiveAccess &&
           "HasExclusiveAccess was changed while the RAII was alive.");
    Arena->HasExclusiveAccess = false;
  }
};

} // namespace syntax
} // namespace swift

#endif
