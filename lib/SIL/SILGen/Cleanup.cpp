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


/// Leave a scope, with all its cleanups.
void CleanupManager::endScope(CleanupsDepth depth) {
  Stack.checkIterator(depth);
  
  // Fast path: no cleanups to leave in this scope.
  if (Stack.stable_begin() == depth) {
    // FIXME.
    //popAndEmitTopDeadCleanups(*this, Cleanups, InnermostScope);
    return;
  }
  
#if 0 // FIXME: Cleanups.
  
  // Thread a branch through the cleanups if there are any active
  // cleanups and we have a valid insertion point.
  BasicBlock *contBB = nullptr;
  if (B.hasValidInsertionPoint() &&
      hasAnyActiveCleanups(Cleanups.begin(), Cleanups.find(depth))) {
    contBB = new (C) BasicBlock(&C, "cleanups.fallthrough");
    emitBranch(JumpDest(contBB, depth));
  }
  
  // Iteratively mark cleanups dead and pop them.
  // Maybe we'd get better results if we marked them all dead in one shot?
  while (Cleanups.stable_begin() != depth) {
    // Mark the cleanup dead.
    if (!Cleanups.begin()->isDead())
      setCleanupState(*Cleanups.begin(), CleanupState::Dead);
    
    // Pop it.
    popAndEmitTopCleanup(*this, Cleanups);
  }
  
  // Emit the continuation block if we made one.
  if (contBB)
    B.emitMergeableBlock(contBB);
#endif
}


/// emitBranchAndCleanups - Emit a branch to the given jump destination,
/// threading out through any cleanups we might need to run.  This does not
/// pop the cleanup stack.
void CleanupManager::emitBranchAndCleanups(JumpDest Dest) {
  CFGBuilder &B = Gen.getBuilder();
  assert(B.hasValidInsertionPoint() && "Inserting branch in invalid spot");
  assert(Stack.empty() && "FIXME: Implement cleanup support");
  B.createBranch(Dest.getBlock());
}
