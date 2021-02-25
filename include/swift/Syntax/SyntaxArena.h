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
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/Allocator.h"

namespace swift {
namespace syntax {

/// Memory manager for Syntax nodes.
class SyntaxArena : public llvm::ThreadSafeRefCountedBase<SyntaxArena> {
  SyntaxArena(const SyntaxArena &) = delete;
  void operator=(const SyntaxArena &) = delete;

  llvm::BumpPtrAllocator Allocator;

  /// \c SyntaxArenas that are kept alive as long as this arena is alive.
  /// If a \c RawSyntax A node in this arena references a \c RawSyntax node B
  /// from a different arena, the B's arena will be added to this list.
  /// The \c SyntaxArena's in this set are manually retained when added to this
  /// list by \c addChildArena and manually released in the destructor of this
  /// arena.
  llvm::SmallPtrSet<SyntaxArena *, 4> ChildArenas;

  /// The start (inclusive) and end (exclusive) pointers of a memory region that
  /// is frequently requested using \c containsPointer. Must be inside \c
  /// Allocator but \c containsPointer will check this region first and will
  /// thus return more quickly for pointers that lie within this region.
  const void *HotUseMemoryRegionStart = nullptr;
  const void *HotUseMemoryRegionEnd = nullptr;

public:
  SyntaxArena() {}

  ~SyntaxArena() {
    // Release all child arenas that were manually retained in addChildArena.
    for (SyntaxArena *ChildArena : ChildArenas) {
      ChildArena->Release();
    }
  }

  static RC<SyntaxArena> make() { return RC<SyntaxArena>(new SyntaxArena()); }

  /// Add an arena that is kept alive while this arena lives. See documentation
  /// of \c ChildArenas for more detail.
  void addChildArena(SyntaxArena *Arena) {
    if (Arena == this) {
      return;
    }
    auto DidInsert = ChildArenas.insert(Arena);
    if (DidInsert.second) {
      Arena->Retain();
    }
  }

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

} // namespace syntax
} // namespace swift

#endif
