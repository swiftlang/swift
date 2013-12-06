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
#include "SILGenFunction.h"
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

namespace {
  /// A CleanupBuffer is a location to which to temporarily copy a
  /// cleanup.
  class CleanupBuffer {
    SmallVector<char, sizeof(Cleanup) + 10 * sizeof(void*)> Data;
    
  public:
    CleanupBuffer(const Cleanup &cleanup) {
      size_t size = cleanup.allocated_size();
      Data.set_size(size);
      memcpy(Data.data(), reinterpret_cast<const void*>(&cleanup), size);
    }
    
    Cleanup &getCopy() { return *reinterpret_cast<Cleanup*>(Data.data()); }
  };
}

void CleanupManager::popAndEmitTopCleanup(CleanupLocation *l) {
  Cleanup &stackCleanup = *Stack.begin();
  
  // Copy it off the cleanups stack.
  CleanupBuffer buffer(stackCleanup);
  Cleanup &cleanup = buffer.getCopy();
  
  // Pop now.
  Stack.pop();

  if (cleanup.isActive() && Gen.B.hasValidInsertionPoint()) {
    assert(l && "Location should be provided for active cleanups.");
    cleanup.emit(Gen, *l);
  }
}

void CleanupManager::popTopDeadCleanups(CleanupsDepth end) {
  Stack.checkIterator(end);

  while (Stack.stable_begin() != end && Stack.begin()->isDead()) {
    assert(!Stack.empty());
    
    popAndEmitTopCleanup(0);
    Stack.checkIterator(end);
  }
}

/// Leave a scope, with all its cleanups.
void CleanupManager::endScope(CleanupsDepth depth, CleanupLocation l) {
  Stack.checkIterator(depth);
  
  // FIXME: Thread a branch through the cleanups if there are any active
  // cleanups and we have a valid insertion point.
  
  if (!hasAnyActiveCleanups(Stack.begin(), Stack.find(depth)))
    return;
  
  // Iteratively mark cleanups dead and pop them.
  // Maybe we'd get better results if we marked them all dead in one shot?
  while (Stack.stable_begin() != depth) {
    popAndEmitTopCleanup(&l);
  }
}


/// emitBranchAndCleanups - Emit a branch to the given jump destination,
/// threading out through any cleanups we might need to run.  This does not
/// pop the cleanup stack.
void CleanupManager::emitBranchAndCleanups(JumpDest Dest,
                                           SILLocation BranchLoc,
                                           ArrayRef<SILValue> Args) {
  SILBuilder &B = Gen.getBuilder();
  assert(B.hasValidInsertionPoint() && "Inserting branch in invalid spot");
  auto depth = Dest.getDepth();
  auto end = Stack.find(depth);
  for (auto cleanup = Stack.begin();
       cleanup != end;
       ++cleanup) {
    if (cleanup->isActive()) {
      cleanup->emit(Gen, Dest.getCleanupLocation());
    }
  }

  B.createBranch(BranchLoc, Dest.getBlock(), Args);
}

void CleanupManager::emitCleanupsForReturn(CleanupLocation Loc) {
  for (auto &cleanup : Stack)
    if (cleanup.isActive())
      cleanup.emit(Gen, Loc);
}

Cleanup &CleanupManager::initCleanup(Cleanup &cleanup,
                                     size_t allocSize,
                                     CleanupState state) {
  cleanup.allocatedSize = allocSize;
  cleanup.state = state;
  return cleanup;
}

void CleanupManager::setCleanupState(CleanupsDepth depth, CleanupState state) {
  auto iter = Stack.find(depth);
  assert(iter != Stack.end() && "can't change end of cleanups stack");
  setCleanupState(*iter, state);
  
  if (state == CleanupState::Dead && iter == Stack.begin())
    popTopDeadCleanups(InnermostScope);
}

void CleanupManager::setCleanupState(Cleanup &cleanup, CleanupState state) {
  assert(Gen.B.hasValidInsertionPoint() &&
         "changing cleanup state at invalid IP");

  // Do the transition now to avoid doing it in N places below.
  CleanupState oldState = cleanup.getState();
  (void)oldState;
  cleanup.setState(state);

  assert(state != oldState && "trivial cleanup state change");
  assert(oldState != CleanupState::Dead && "changing state of dead cleanup");

  // Our current cleanup emission logic, where we don't try to re-use
  // cleanup emissions between various branches, doesn't require any
  // code to be emitted at transition points.
}
