//===--- Cleanup.cpp - Implements the Cleanup mechanics --------------------==//
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

#include "Cleanup.h"
#include "SILGen.h"
using namespace swift;
using namespace Lowering;

/// Are there any active cleanups in the given range?
static bool hasAnyActiveCleanups(DiverseStackImpl<Cleanup>::iterator begin,
                                 DiverseStackImpl<Cleanup>::iterator end) {
  for (; begin != end; ++begin)
    if (begin->isActive())
      return true;
  return false;
}

/// Transition the state of a cleanup.
/// FIXME: transition logic
static void setCleanupState(Cleanup &cleanup,
                            CleanupState state) {
  cleanup.setState(state);
}

namespace {
  /// A CleanupBuffer is a location to which to temporarily copy a
  /// cleanup.
  class CleanupBuffer {
    llvm::SmallVector<char, sizeof(Cleanup) + 10 * sizeof(void*)> Data;
    
  public:
    CleanupBuffer(const Cleanup &cleanup) {
      size_t size = cleanup.allocated_size();
      Data.set_size(size);
      memcpy(Data.data(), reinterpret_cast<const void*>(&cleanup), size);
    }
    
    Cleanup &getCopy() { return *reinterpret_cast<Cleanup*>(Data.data()); }
  };
}

void CleanupManager::popAndEmitTopCleanup() {
  Cleanup &stackCleanup = *Stack.begin();
  assert(stackCleanup.isDead() && "popping a living cleanup");
  
  // Copy it off the cleanups stack.
  CleanupBuffer buffer(stackCleanup);
  Cleanup &cleanup = buffer.getCopy();
  
  // Pop now.
  Stack.pop();
  
  cleanup.emit(Gen);
}

/// Leave a scope, with all its cleanups.
void CleanupManager::endScope(CleanupsDepth depth) {
  Stack.checkIterator(depth);
  
  // FIXME: Thread a branch through the cleanups if there are any active
  // cleanups and we have a valid insertion point.
  
  if (!hasAnyActiveCleanups(Stack.begin(), Stack.find(depth)))
    return;
  
  // Iteratively mark cleanups dead and pop them.
  // Maybe we'd get better results if we marked them all dead in one shot?
  while (Stack.stable_begin() != depth) {
    // Mark the cleanup dead.
    if (!Stack.begin()->isDead())
      setCleanupState(*Stack.begin(), CleanupState::Dead);
    
    // Pop it.
    popAndEmitTopCleanup();
  }
}


/// emitBranchAndCleanups - Emit a branch to the given jump destination,
/// threading out through any cleanups we might need to run.  This does not
/// pop the cleanup stack.
void CleanupManager::emitBranchAndCleanups(JumpDest Dest) {
  SILBuilder &B = Gen.getBuilder();
  assert(B.hasValidInsertionPoint() && "Inserting branch in invalid spot");
  auto depth = Dest.getDepth();
  auto end = Stack.find(depth);
  for (auto cleanup = Stack.begin();
       cleanup != end;
       ++cleanup) {
    cleanup->emit(Gen);
  }
  B.createBranch(Dest.getBlock());
}

Cleanup &CleanupManager::initCleanup(Cleanup &cleanup,
                                     size_t allocSize,
                                     CleanupState state)
{
  cleanup.allocatedSize = allocSize;
  cleanup.state = state;
  return cleanup;
}
