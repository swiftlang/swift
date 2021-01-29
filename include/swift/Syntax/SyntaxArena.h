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
class SyntaxArena : public llvm::ThreadSafeRefCountedBase<SyntaxArena> {
  SyntaxArena(const SyntaxArena &) = delete;
  void operator=(const SyntaxArena &) = delete;

  llvm::BumpPtrAllocator Allocator;

  /// The start (inclusive) and end (exclusive) pointers of a memory region that
  /// is frequently requested using \c containsPointer. Must be inside \c
  /// Allocator but \c containsPointer will check this region first and will
  /// thus return more quickly for pointers that lie within this region.
  const void *HotUseMemoryRegionStart = nullptr;
  const void *HotUseMemoryRegionEnd = nullptr;

public:
  SyntaxArena() {}

  static RC<SyntaxArena> make() { return RC<SyntaxArena>(new SyntaxArena()); }

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
