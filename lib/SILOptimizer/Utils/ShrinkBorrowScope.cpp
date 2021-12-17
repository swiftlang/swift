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
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SILOptimizer/Utils/CanonicalizeBorrowScope.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"

#define DEBUG_TYPE "copy-propagation"

using namespace swift;

//===----------------------------------------------------------------------===//
//                           MARK: Local utilities
//===----------------------------------------------------------------------===//

// TODO: Move to be member function on SILInstruction.
static SILInstruction *getPreviousInstruction(SILInstruction *inst) {
  auto pos = inst->getIterator();
  return pos == inst->getParent()->begin() ? nullptr
                                           : &*std::prev(inst->getIterator());
}

// TODO: Move to be member function on SILInstruction.
static SILInstruction *getNextInstruction(SILInstruction *inst) {
  auto nextPos = std::next(inst->getIterator());
  return nextPos == inst->getParent()->end() ? nullptr : &*nextPos;
}

//===----------------------------------------------------------------------===//
//                       MARK: ShrinkBorrowScope
//===----------------------------------------------------------------------===//

class ShrinkBorrowScope {
  // The instruction that begins this borrow scope.
  BeginBorrowInst *introducer;

  InstructionDeleter &deleter;

  SmallVectorImpl<CopyValueInst *> &modifiedCopyValueInsts;

  SmallPtrSet<SILInstruction *, 16> users;
  llvm::SmallVector<std::pair<SILBasicBlock *, SILInstruction *>>
      barrierInstructions;

  SmallPtrSet<SILBasicBlock *, 8> blocksToEndAtTop;

  llvm::SmallDenseMap<ApplySite, size_t> transitiveUsesPerApplySite;

  // The list of blocks to look for new points at which to insert end_borrows
  // in.  A block must not be processed if all of its successors have not yet
  // been.  For that reason, it is necessary to allow the same block to be
  // visited multiple times, at most once for each successor.
  SmallVector<SILBasicBlock *, 8> worklist;
  // The instructions from which the shrinking starts, the scope ending
  // instructions, keyed off the block in which they appear.
  llvm::SmallDenseMap<SILBasicBlock *, SILInstruction *> startingInstructions;

public:
  ShrinkBorrowScope(BeginBorrowInst *bbi, InstructionDeleter &deleter,
                    SmallVectorImpl<CopyValueInst *> &modifiedCopyValueInsts)
      : introducer(bbi), deleter(deleter),
        modifiedCopyValueInsts(modifiedCopyValueInsts) {}

  bool run();

  bool populateUsers();
  bool initializeWorklist();
  void findBarriers();
  void rewrite();
  void createEndBorrow(SILInstruction *insertionPoint);

  bool isBarrierApply(SILInstruction *instruction) {
    // For now, treat every apply (that doesn't use the borrowed value) as a
    // barrier.
    return isa<ApplySite>(instruction);
  }

  bool mayAccessPointer(SILInstruction *instruction) {
    if (!instruction->mayReadOrWriteMemory())
      return false;
    bool fail = false;
    visitAccessedAddress(instruction, [&fail](Operand *operand) {
      auto accessStorage = AccessStorage::compute(operand->get());
      if (accessStorage.getKind() != AccessRepresentation::Kind::Unidentified)
        fail = true;
    });
    return fail;
  }

  bool mayLoadWeakOrUnowned(SILInstruction *instruction) {
    // TODO: It is possible to do better here by looking at the address that is
    //       being loaded.
    return isa<LoadWeakInst>(instruction) || isa<LoadUnownedInst>(instruction);
  }

  bool isDeinitBarrier(SILInstruction *instruction) {
    return isBarrierApply(instruction) || instruction->maySynchronize() ||
           mayAccessPointer(instruction) || mayLoadWeakOrUnowned(instruction);
  }

  bool canReplaceValueWithBorrowedValue(SILValue value) {
    while (true) {
      auto *instruction = value.getDefiningInstruction();
      if (!instruction)
        return false;
      if (auto *cvi = dyn_cast<CopyValueInst>(instruction)) {
        value = cvi->getOperand();
        continue;
      } else if (auto *bbi = dyn_cast<BeginBorrowInst>(instruction)) {
        if (bbi == introducer) {
          return true;
        }
      }
      return false;
    }
  }

  size_t usesInApply(ApplySite apply) {
    if (auto count = transitiveUsesPerApplySite.lookup(apply))
      return count;
    return 0;
  }

  bool tryHoistOverInstruction(SILInstruction *instruction) {
    if (users.contains(instruction)) {
      if (auto apply = ApplySite::isa(instruction)) {
        SmallVector<int, 2> rewritableArgumentIndices;
        auto count = apply.getNumArguments();
        for (unsigned index = 0; index < count; ++index) {
          auto argument = apply.getArgument(index);
          if (canReplaceValueWithBorrowedValue(argument)) {
            rewritableArgumentIndices.push_back(index);
          }
        }
        if (rewritableArgumentIndices.size() != usesInApply(apply)) {
          return false;
        }
        // We can rewrite all the arguments which are transitive uses of the
        // borrow.
        for (auto index : rewritableArgumentIndices) {
          auto argument = apply.getArgument(index);
          auto borrowee = introducer->getOperand();
          if (auto *cvi = dyn_cast<CopyValueInst>(argument)) {
            cvi->setOperand(borrowee);
            modifiedCopyValueInsts.push_back(cvi);
          } else {
            apply.setArgument(index, borrowee);
          }
        }
        return true;
      } else if (auto *bbi = dyn_cast<BeginBorrowInst>(instruction)) {
        if (bbi->isLexical() &&
            canReplaceValueWithBorrowedValue(bbi->getOperand())) {
          auto borrowee = introducer->getOperand();
          bbi->setOperand(borrowee);
          return true;
        }
      } else if (auto *cvi = dyn_cast<CopyValueInst>(instruction)) {
        if (canReplaceValueWithBorrowedValue(cvi->getOperand())) {
          auto borrowee = introducer->getOperand();
          cvi->setOperand(borrowee);
          modifiedCopyValueInsts.push_back(cvi);
          return true;
        }
      }
      return false;
    }
    return !isDeinitBarrier(instruction);
  }
};

//===----------------------------------------------------------------------===//
//                        MARK: Rewrite borrow scopes
//===----------------------------------------------------------------------===//

bool ShrinkBorrowScope::run() {
  if (!BorrowedValue(introducer).isLocalScope())
    return false;
  if (!populateUsers())
    return false;
  if (!initializeWorklist())
    return false;

  findBarriers();

  rewrite();

  return true;
}

bool ShrinkBorrowScope::populateUsers() {
  SmallVector<Operand *, 16> uses;
  if (!findExtendedUsesOfSimpleBorrowedValue(BorrowedValue(introducer),
                                             &uses)) {
    // If the value produced by begin_borrow escapes, don't shrink the borrow
    // scope.
    return false;
  }
  for (auto *use : uses) {
    auto *user = use->getUser();
    users.insert(user);
    if (auto apply = ApplySite::isa(user)) {
      ++transitiveUsesPerApplySite[apply];
    }
  }
  return true;
}

bool ShrinkBorrowScope::initializeWorklist() {
  llvm::SmallVector<SILInstruction *, 16> scopeEndingInsts;
  BorrowedValue(introducer).getLocalScopeEndingInstructions(scopeEndingInsts);

  // Form a map of the scopeEndingInsts, keyed off the block they occur in.  If
  // a scope ending instruction is not an end_borrow, bail out.
  for (auto *instruction : scopeEndingInsts) {
    if (!isa<EndBorrowInst>(instruction))
      return false;
    auto *block = instruction->getParent();
    worklist.push_back(block);
    startingInstructions[block] = instruction;
  }

  return true;
}

void ShrinkBorrowScope::findBarriers() {
  // Walk the cfg backwards from the blocks containing scope ending
  // instructions, visiting only the initial blocks (which contained those
  // instructions) and those blocks all of whose successors have already been
  // visited.
  //
  // TODO: Handle loops.
  SmallPtrSet<SILBasicBlock *, 8> blocksWithReachedTops;
  auto reachedTopOfAllSuccessors =
      [&blocksWithReachedTops](SILBasicBlock *block) -> bool {
    return llvm::all_of(block->getSuccessorBlocks(), [=](auto *successor) {
      return blocksWithReachedTops.contains(successor);
    });
  };

  while (!worklist.empty()) {
    auto *block = worklist.pop_back_val();
    auto *startingInstruction = startingInstructions.lookup(block);
    if (!startingInstruction && !reachedTopOfAllSuccessors(block)) {
      continue;
    }
    if (!startingInstruction &&
        !tryHoistOverInstruction(block->getTerminator())) {
      // This block was walked to--it was not one containing one of the initial
      // end_borrow instructions.  Check whether it forwards the ownership of
      // the borrowed value (either directly or indirectly).  If it does, we
      // must not hoist the end_borrow above it.
      continue;
    }
    for (auto *successor : block->getSuccessorBlocks()) {
      blocksToEndAtTop.erase(successor);
    }

    // We either have processed all successors of block or else it is a block
    // which contained one of the original scope-ending instructions.  Scan the
    // block backwards, looking for the first deinit barrier.  If we've visited
    // all successors, start scanning from the terminator.  If the block
    // contained an original scope-ending instruction, start scanning from it.
    SILInstruction *instruction =
        startingInstruction ? startingInstruction : block->getTerminator();
    SILInstruction *barrier = nullptr;
    while ((instruction = getPreviousInstruction(instruction))) {
      if (instruction == introducer) {
        barrier = instruction;
        break;
      }
      if (!tryHoistOverInstruction(instruction)) {
        barrier = instruction;
        break;
      }
    }

    if (barrier) {
      barrierInstructions.push_back({block, barrier});
    } else {
      blocksWithReachedTops.insert(block);
      blocksToEndAtTop.insert(block);
      for (auto *predecessor : block->getPredecessorBlocks()) {
        worklist.push_back(predecessor);
      }
    }
  }
}

void ShrinkBorrowScope::rewrite() {
  // Remove all the original end_borrow instructions.
  for (auto pair : startingInstructions) {
    deleter.forceDelete(pair.getSecond());
  }

  // Insert the new end_borrow instructions that occur after deinit barriers.
  for (auto pair : barrierInstructions) {
    auto *insertionPoint = getNextInstruction(pair.second);
    createEndBorrow(insertionPoint);
  }

  // Insert the new end_borrow instructions that occur at the beginning of
  // blocks which we couldn't hoist out of.
  for (auto *block : blocksToEndAtTop) {
    auto *insertionPoint = &*block->begin();
    createEndBorrow(insertionPoint);
  }
}

void ShrinkBorrowScope::createEndBorrow(SILInstruction *insertionPoint) {
  auto builder = SILBuilderWithScope(insertionPoint);
  builder.createEndBorrow(
      RegularLocation::getAutoGeneratedLocation(insertionPoint->getLoc()),
      introducer);
}

bool swift::shrinkBorrowScope(
    BeginBorrowInst *bbi, InstructionDeleter &deleter,
    SmallVectorImpl<CopyValueInst *> &modifiedCopyValueInsts) {
  ShrinkBorrowScope borrowShrinker(bbi, deleter, modifiedCopyValueInsts);
  return borrowShrinker.run();
}
