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
    SmallVector<char, sizeof(Cleanup) + 10 * sizeof(void *)> data;

  public:
    CleanupBuffer(const Cleanup &cleanup) {
      size_t size = cleanup.allocated_size();
      data.set_size(size);
      memcpy(data.data(), reinterpret_cast<const void *>(&cleanup), size);
    }

    Cleanup &getCopy() { return *reinterpret_cast<Cleanup *>(data.data()); }
  };
} // end anonymous namespace

void CleanupManager::popTopDeadCleanups(CleanupsDepth end) {
  stack.checkIterator(end);

  while (stack.stable_begin() != end && stack.begin()->isDead()) {
    assert(!stack.empty());
    stack.pop();
    stack.checkIterator(end);
  }
}

void CleanupManager::emitCleanups(CleanupsDepth depth, CleanupLocation loc,
                                  bool popCleanups) {
  auto begin = stack.stable_begin();
  while (begin != depth) {
    auto iter = stack.find(begin);

    Cleanup &stackCleanup = *iter;

    // Copy it off the cleanup stack in case the cleanup pushes a new cleanup
    // and the backing storage is re-allocated.
    CleanupBuffer buffer(stackCleanup);
    Cleanup &cleanup = buffer.getCopy();

    // Advance stable iterator.
    begin = stack.stabilize(++iter);

    // Pop now.
    if (popCleanups)
      stack.pop();

    if (cleanup.isActive() && SGF.B.hasValidInsertionPoint())
      cleanup.emit(SGF, loc);

    stack.checkIterator(begin);
  }
}

/// Leave a scope, with all its cleanups.
void CleanupManager::endScope(CleanupsDepth depth, CleanupLocation loc) {
  stack.checkIterator(depth);

  // FIXME: Thread a branch through the cleanups if there are any active
  // cleanups and we have a valid insertion point.

  if (!::hasAnyActiveCleanups(stack.begin(), stack.find(depth))) {
    return;
  }
  
  // Iteratively mark cleanups dead and pop them.
  // Maybe we'd get better results if we marked them all dead in one shot?
  emitCleanups(depth, loc);
}

bool CleanupManager::hasAnyActiveCleanups(CleanupsDepth from,
                                          CleanupsDepth to) {
  return ::hasAnyActiveCleanups(stack.find(from), stack.find(to));
}

bool CleanupManager::hasAnyActiveCleanups(CleanupsDepth from) {
  return ::hasAnyActiveCleanups(stack.begin(), stack.find(from));
}

/// emitBranchAndCleanups - Emit a branch to the given jump destination,
/// threading out through any cleanups we might need to run.  This does not
/// pop the cleanup stack.
void CleanupManager::emitBranchAndCleanups(JumpDest dest, SILLocation branchLoc,
                                           ArrayRef<SILValue> args) {
  SILGenBuilder &builder = SGF.getBuilder();
  assert(builder.hasValidInsertionPoint() && "Emitting branch in invalid spot");
  emitCleanups(dest.getDepth(), dest.getCleanupLocation(),
               /*popCleanups=*/false);
  builder.createBranch(branchLoc, dest.getBlock(), args);
}

void CleanupManager::emitCleanupsForReturn(CleanupLocation loc) {
  SILGenBuilder &builder = SGF.getBuilder();
  assert(builder.hasValidInsertionPoint() && "Emitting return in invalid spot");
  (void)builder;
  emitCleanups(stack.stable_end(), loc, /*popCleanups=*/false);
}

/// Emit a new block that jumps to the specified location and runs necessary
/// cleanups based on its level.  If there are no cleanups to run, this just
/// returns the dest block.
SILBasicBlock *CleanupManager::emitBlockForCleanups(JumpDest dest,
                                                    SILLocation branchLoc,
                                                    ArrayRef<SILValue> args) {
  // If there are no cleanups to run, just return the Dest block directly.
  if (!hasAnyActiveCleanups(dest.getDepth()))
    return dest.getBlock();

  // Otherwise, create and emit a new block.
  auto *newBlock = SGF.createBasicBlock();
  SavedInsertionPoint IPRAII(SGF, newBlock);
  emitBranchAndCleanups(dest, branchLoc, args);
  return newBlock;
}


Cleanup &CleanupManager::initCleanup(Cleanup &cleanup,
                                     size_t allocSize,
                                     CleanupState state) {
  cleanup.allocatedSize = allocSize;
  cleanup.state = state;
  return cleanup;
}

void CleanupManager::setCleanupState(CleanupsDepth depth, CleanupState state) {
  auto iter = stack.find(depth);
  assert(iter != stack.end() && "can't change end of cleanups stack");
  setCleanupState(*iter, state);

  if (state == CleanupState::Dead && iter == stack.begin())
    popTopDeadCleanups(innermostScope);
}

void CleanupManager::forwardCleanup(CleanupsDepth handle) {
  auto iter = stack.find(handle);
  assert(iter != stack.end() && "can't change end of cleanups stack");
  Cleanup &cleanup = *iter;
  assert(cleanup.isActive() && "forwarding inactive or dead cleanup?");

  CleanupState newState = (cleanup.getState() == CleanupState::Active
                             ? CleanupState::Dead
                             : CleanupState::Dormant);
  setCleanupState(cleanup, newState);

  if (newState == CleanupState::Dead && iter == stack.begin())
    popTopDeadCleanups(innermostScope);
}

void CleanupManager::setCleanupState(Cleanup &cleanup, CleanupState state) {
  assert(SGF.B.hasValidInsertionPoint() &&
         "changing cleanup state at invalid IP");

  // Do the transition now to avoid doing it in N places below.
  CleanupState oldState = cleanup.getState();
  (void)oldState;
  cleanup.setState(SGF, state);

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

  auto iter = cleanups.stack.find(handle);
  assert(iter != cleanups.stack.end() && "can't change end of cleanups stack");
  Cleanup &cleanup = *iter;
  assert(cleanup.getState() != CleanupState::Dead &&
         "changing state of dead cleanup");

  CleanupState oldState = cleanup.getState();
  cleanup.setState(cleanups.SGF, newState);

  savedStates.push_back({handle, oldState});
}

void
CleanupStateRestorationScope::pushCurrentCleanupState(CleanupHandle handle) {
  auto iter = cleanups.stack.find(handle);
  assert(iter != cleanups.stack.end() && "can't change end of cleanups stack");
  Cleanup &cleanup = *iter;
  assert(cleanup.getState() != CleanupState::Dead &&
         "changing state of dead cleanup");

  CleanupState oldState = cleanup.getState();
  savedStates.push_back({handle, oldState});
}

void CleanupStateRestorationScope::popImpl() {
  // Restore cleanup states in the opposite order in which we saved them.
  for (auto i = savedStates.rbegin(), e = savedStates.rend(); i != e; ++i) {
    CleanupHandle handle = i->first;
    CleanupState stateToRestore = i->second;

    auto iter = cleanups.stack.find(handle);
    assert(iter != cleanups.stack.end() &&
           "can't change end of cleanups stack");
    Cleanup &cleanup = *iter;
    assert(cleanup.getState() != CleanupState::Dead &&
           "changing state of dead cleanup");
    cleanup.setState(cleanups.SGF, stateToRestore);
  }

  savedStates.clear();
}

void CleanupStateRestorationScope::pop() && { popImpl(); }

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
  auto begin = stack.stable_begin();
  auto end = stack.stable_end();
  while (begin != end) {
    auto iter = stack.find(begin);
    const Cleanup &stackCleanup = *iter;
    llvm::errs() << "CLEANUP DEPTH: " << begin.getDepth() << "\n";
    stackCleanup.dump(SGF);
    begin = stack.stabilize(++iter);
    stack.checkIterator(begin);
  }
#endif
}

void CleanupManager::dump(CleanupHandle handle) const {
  auto iter = stack.find(handle);
  const Cleanup &stackCleanup = *iter;
  llvm::errs() << "CLEANUP DEPTH: " << handle.getDepth() << "\n";
  stackCleanup.dump(SGF);
}

void CleanupManager::checkIterator(CleanupHandle handle) const {
#ifndef NDEBUG
  stack.checkIterator(handle);
#endif
}
