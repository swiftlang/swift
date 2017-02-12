//===--- Cleanup.cpp - Implements the Cleanup mechanics -------------------===//
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
} // end anonymous namespace

void CleanupManager::popTopDeadCleanups(CleanupsDepth end) {
  Stack.checkIterator(end);

  while (Stack.stable_begin() != end && Stack.begin()->isDead()) {
    assert(!Stack.empty());
    Stack.pop();
    Stack.checkIterator(end);
  }
}

void CleanupManager::emitCleanups(CleanupsDepth depth, CleanupLocation l,
                                  bool popCleanups) {
  auto begin = Stack.stable_begin();
  while (begin != depth) {
    auto iter = Stack.find(begin);

    Cleanup &stackCleanup = *iter;

    // Copy it off the cleanup stack in case the cleanup pushes a new cleanup
    // and the backing storage is re-allocated.
    CleanupBuffer buffer(stackCleanup);
    Cleanup &cleanup = buffer.getCopy();

    // Advance stable iterator.
    begin = Stack.stabilize(++iter);

    // Pop now.
    if (popCleanups)
      Stack.pop();

    if (cleanup.isActive() && Gen.B.hasValidInsertionPoint())
      cleanup.emit(Gen, l);

    Stack.checkIterator(begin);
  }
}

/// Leave a scope, with all its cleanups.
void CleanupManager::endScope(CleanupsDepth depth, CleanupLocation l) {
  Stack.checkIterator(depth);
  
  // FIXME: Thread a branch through the cleanups if there are any active
  // cleanups and we have a valid insertion point.
  
  if (!::hasAnyActiveCleanups(Stack.begin(), Stack.find(depth))) {
    return;
  }
  
  // Iteratively mark cleanups dead and pop them.
  // Maybe we'd get better results if we marked them all dead in one shot?
  emitCleanups(depth, l);
}

bool CleanupManager::hasAnyActiveCleanups(CleanupsDepth from,
                                          CleanupsDepth to) {
  return ::hasAnyActiveCleanups(Stack.find(from), Stack.find(to));
}

bool CleanupManager::hasAnyActiveCleanups(CleanupsDepth from) {
  return ::hasAnyActiveCleanups(Stack.begin(), Stack.find(from));
}

/// emitBranchAndCleanups - Emit a branch to the given jump destination,
/// threading out through any cleanups we might need to run.  This does not
/// pop the cleanup stack.
void CleanupManager::emitBranchAndCleanups(JumpDest Dest,
                                           SILLocation BranchLoc,
                                           ArrayRef<SILValue> Args) {
  SILGenBuilder &B = Gen.getBuilder();
  assert(B.hasValidInsertionPoint() && "Emitting branch in invalid spot");
  emitCleanups(Dest.getDepth(), Dest.getCleanupLocation(), /*popCleanups=*/false);
  B.createBranch(BranchLoc, Dest.getBlock(), Args);
}

void CleanupManager::emitCleanupsForReturn(CleanupLocation Loc) {
  SILGenBuilder &B = Gen.getBuilder();
  assert(B.hasValidInsertionPoint() && "Emitting return in invalid spot");
  (void) B;
  emitCleanups(Stack.stable_end(), Loc, /*popCleanups=*/false);
}

/// Emit a new block that jumps to the specified location and runs necessary
/// cleanups based on its level.  If there are no cleanups to run, this just
/// returns the dest block.
SILBasicBlock *CleanupManager::emitBlockForCleanups(JumpDest Dest,
                                                    SILLocation BranchLoc,
                                                    ArrayRef<SILValue> Args) {
  // If there are no cleanups to run, just return the Dest block directly.
  if (!hasAnyActiveCleanups(Dest.getDepth()))
    return Dest.getBlock();

  // Otherwise, create and emit a new block.
  auto *NewBB = Gen.createBasicBlock();
  SavedInsertionPoint IPRAII(Gen, NewBB);
  emitBranchAndCleanups(Dest, BranchLoc, Args);
  return NewBB;
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

void CleanupManager::forwardCleanup(CleanupsDepth handle) {
  auto iter = Stack.find(handle);
  assert(iter != Stack.end() && "can't change end of cleanups stack");
  Cleanup &cleanup = *iter;
  assert(cleanup.isActive() && "forwarding inactive or dead cleanup?");

  CleanupState newState = (cleanup.getState() == CleanupState::Active
                             ? CleanupState::Dead
                             : CleanupState::Dormant);
  setCleanupState(cleanup, newState);

  if (newState == CleanupState::Dead && iter == Stack.begin())
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

void CleanupStateRestorationScope::pushCleanupState(CleanupHandle handle,
                                                    CleanupState newState) {
  // Don't put the cleanup in a state we can't restore it from.
  assert(newState != CleanupState::Dead && "cannot restore cleanup from death");

  auto iter = Cleanups.Stack.find(handle);
  assert(iter != Cleanups.Stack.end() && "can't change end of cleanups stack");
  Cleanup &cleanup = *iter;
  assert(cleanup.getState() != CleanupState::Dead &&
         "changing state of dead cleanup");

  CleanupState oldState = cleanup.getState();
  cleanup.setState(newState);

  SavedStates.push_back({handle, oldState});
}

void
CleanupStateRestorationScope::pushCurrentCleanupState(CleanupHandle handle) {
  auto iter = Cleanups.Stack.find(handle);
  assert(iter != Cleanups.Stack.end() && "can't change end of cleanups stack");
  Cleanup &cleanup = *iter;
  assert(cleanup.getState() != CleanupState::Dead &&
         "changing state of dead cleanup");

  CleanupState oldState = cleanup.getState();
  SavedStates.push_back({handle, oldState});
}

void CleanupStateRestorationScope::pop() {
  // Restore cleanup states in the opposite order in which we saved them.
  for (auto i = SavedStates.rbegin(), e = SavedStates.rend(); i != e; ++i) {
    CleanupHandle handle = i->first;
    CleanupState stateToRestore = i->second;

    auto iter = Cleanups.Stack.find(handle);
    assert(iter != Cleanups.Stack.end() && "can't change end of cleanups stack");
    Cleanup &cleanup = *iter;
    assert(cleanup.getState() != CleanupState::Dead &&
           "changing state of dead cleanup");
    cleanup.setState(stateToRestore);
  }
}

llvm::raw_ostream &Lowering::operator<<(llvm::raw_ostream &os,
                                        CleanupState state) {
  switch (state) {
  case CleanupState::Dormant:
    return os << "Dormant";
  case CleanupState::Dead:
    return os << "Dead";
  case CleanupState::Active:
    return os << "Active";
  case CleanupState::PersistentlyActive:
    return os << "PersistentlyActive";
  }

  llvm_unreachable("Unhandled CleanupState in switch.");
}

void CleanupManager::dump() const {
#ifndef NDEBUG
  auto begin = Stack.stable_begin();
  auto end = Stack.stable_end();
  while (begin != end) {
    auto iter = Stack.find(begin);
    const Cleanup &stackCleanup = *iter;
    llvm::errs() << "CLEANUP DEPTH: " << begin.getDepth() << "\n";
    stackCleanup.dump();
    begin = Stack.stabilize(++iter);
    Stack.checkIterator(begin);
  }
#endif
}
