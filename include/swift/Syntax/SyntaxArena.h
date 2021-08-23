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
#include "llvm/ADT/STLExtras.h"
#include <set>

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
      assert(!Arena->containsReferenceCycle({this}));
    }
  }

#ifndef NDEBUG
  /// Check if there are any reference cycles in the child arenas. This is done
  /// by walking all child nodes from a root node, collecting all visited nodes
  /// in \p VisitedArenas. If we find a node twice, there's a reference cycle.
  bool
  containsReferenceCycle(std::set<const SyntaxArena *> VisitedArenas = {}) const {
    if (!VisitedArenas.insert(this).second) {
      // this was already in VisitedArenas -> we have a reference cycle
      return true;
    }
    return llvm::any_of(ChildArenas, [&](const SyntaxArena *Child) {
      return Child->containsReferenceCycle(VisitedArenas);
    });
  }
#endif

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

  /// If the \p Data is not allocated in this arena, copy it to this and adjust
  /// \p Data to point to the string's copy in this arena.
  void copyStringToArenaIfNecessary(const char *&Data, size_t Length) {
    if (Length == 0) {
      // Empty strings can live wherever they want. Nothing to do.
      return;
    }
    if (containsPointer(Data)) {
      // String already in arena. Nothing to do.
      return;
    }
    // Copy string to arena
    char *ArenaData = (char *)getAllocator().Allocate<char>(Length);
    std::memcpy(ArenaData, Data, Length);
    Data = ArenaData;
  }
};

} // namespace syntax
} // namespace swift

#endif
