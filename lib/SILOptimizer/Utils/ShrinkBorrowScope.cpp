//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//

#include "swift/AST/Builtins.h"
#define DEBUG_TYPE "copy-propagation"

#include "swift/SIL/SILBasicBlock.h"
#include "swift/SILOptimizer/Utils/ShrinkBorrowScope.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                           MARK: Local utilities
//===----------------------------------------------------------------------===//

// TODO: Move to be member function on SILInstruction.
static SILInstruction *getPreviousInstruction(SILInstruction *inst) {
  auto *block = inst->getParent();
  if (inst == &*block->begin()) {
    return nullptr;
  }
  return &*std::prev(inst->getIterator());
};

// TODO: Move to be member function on SILInstruction.
static SILInstruction *getNextInstruction(SILInstruction *inst) {
  auto *block = inst->getParent();
  if (inst == &*block->getTerminator()) {
    return nullptr;
  }
  return &*std::next(inst->getIterator());
};

//===----------------------------------------------------------------------===//
//                        MARK: Rewrite borrow scopes
//===----------------------------------------------------------------------===//

bool ShrinkBorrowScope::shrinkBorrowScope(BorrowedValue borrow) {
  if (!borrow.isLocalScope())
    return false;
  llvm::SmallVector<SILInstruction *, 16> scopeEndingInsts;
  borrow.getLocalScopeEndingInstructions(scopeEndingInsts);

  SmallVector<Operand *, 16> usePoints;
  SmallPtrSet<SILInstruction *, 16> users;
  for (auto *usePoint : usePoints) {
    users.insert(usePoint->getUser());
  }
  auto isDeinitBarrier = [&](SILInstruction *instruction,
                             SILValue value) -> bool {
    // TODO: Implement this to mean
    //           maySynchronize || mayAccessPointer || mayLoadWeak.
    return true;
  };

  // The list of blocks to look for new points at which to insert end_borrows
  // in.  A block must not be processed if all of its successors have not yet
  // been.  For that reason, it is necessary to allow the same block to be
  // visited multiple times, at most once for each predecessor.
  SmallVector<SILBasicBlock *, 8> worklist;
  SmallPtrSet<SILBasicBlock *, 8> blocksWithReachedTops;
  SmallPtrSet<SILBasicBlock *, 8> blocksToEndAtTop;
  auto reachedTopOfAllSuccessors = [&](SILBasicBlock *block) {
    return llvm::all_of(block->getSuccessorBlocks(), [=](auto *successor) {
      return blocksWithReachedTops.contains(successor);
    });
  };

  // Form a map of the scopeEndingInsts, keyed off the block they occur in.  If
  // a scope ending instruction is not an end_borrow, bail out.
  llvm::SmallDenseMap<SILBasicBlock *, SILInstruction *> startingInstructions;
  for (auto *instruction : scopeEndingInsts) {
    if (!isa<EndBorrowInst>(instruction))
      return false;
    auto *block = instruction->getParent();
    worklist.push_back(block);
    startingInstructions[block] = instruction;
  }

  // Walk the cfg backwards from the blocks containing scope ending
  // instructions, visiting only the initial blocks (which contained those
  // instructions) and those blocks all of whose successors have already been
  // visited.
  //
  // TODO: Handle loops.
  llvm::SmallDenseMap<SILBasicBlock *, SILInstruction *> barrierInstructions;
  while (!worklist.empty()) {
    auto *block = worklist.pop_back_val();
    auto *startingInstruction = startingInstructions.lookup(block);
    if (!startingInstruction && !reachedTopOfAllSuccessors(block)) {
      continue;
    }
    if (!startingInstruction) {
      // This block was walked to--it was not one containing one of the initial
      // end_borrow instructions.  Check whether it forwards the ownership of
      // the borrowed value (either directly or indirectly).  If it does, we
      // must not hoist the end_borrow above it.
      //
      // TODO: Do this without forming the list of all guaranteed reference
      //       roots--we don't need the list and we don't want to keep looking
      //       once we see that a root matches borrow.value.
      TermInst *terminator = block->getTerminator();
      auto operandValues =
          terminator->getOperandValues(/*skipTypeDependentOperands=*/true);
      bool bail = false;
      for (auto operand : operandValues) {
        SmallVector<SILValue, 4> roots;
        findGuaranteedReferenceRoots(operand, roots);
        if (llvm::find(roots, borrow.value) != roots.end()) {
          bail = true;
          break;
        }
      }
      if (bail) {
        continue;
      }
    }
    for (auto *successor : block->getSuccessorBlocks()) {
      blocksToEndAtTop.erase(successor);
    }

    // We either have processed all successors of block or else it is a block
    // which contained one of the original scope-ending instructions.  Scan the
    // block backwards, looking for the first deinit barrier.  If we've visited
    // all successors, start scanning from the terminator.  If block contained
    // an original scope-ending instruction, start scanning from it.
    SILInstruction *instruction =
        startingInstruction ? startingInstruction : block->getTerminator();
    SILInstruction *barrier = nullptr;
    while ((instruction = getPreviousInstruction(instruction))) {
      if (instruction == borrow->getDefiningInstruction()) {
        barrier = instruction;
      }
      if (isDeinitBarrier(instruction, borrow.value)) {
        barrier = instruction;
        break;
      }
    }

    if (barrier) {
      barrierInstructions[block] = barrier;
    } else {
      blocksWithReachedTops.insert(block);
      blocksToEndAtTop.insert(block);
      for (auto *predecessor : block->getPredecessorBlocks()) {
        worklist.push_back(predecessor);
      }
    }
  }

  // Remove all the original end_borrow instructions.
  for (auto pair : startingInstructions) {
    deleter.forceDelete(pair.getSecond());
  }

  // Insert the new end_borrow instructions that occur after deinit barriers.
  for (auto pair : barrierInstructions) {
    auto *insertionPoint = getNextInstruction(pair.getSecond());
    auto builder = SILBuilderWithScope(insertionPoint);
    builder.createEndBorrow(
        RegularLocation::getAutoGeneratedLocation(insertionPoint->getLoc()),
        borrow.value);
  }

  // Insert the new end_borrow instructions that occur at the beginning of
  // blocks which we couldn't hoist out of.
  for (auto *block : blocksToEndAtTop) {
    auto *insertionPoint = &*block->begin();
    auto builder = SILBuilderWithScope(insertionPoint);
    builder.createEndBorrow(
        RegularLocation::getAutoGeneratedLocation(insertionPoint->getLoc()),
        borrow.value);
  }

  return true;
}
