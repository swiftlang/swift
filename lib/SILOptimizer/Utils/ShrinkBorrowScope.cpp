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
//                       MARK: ShrinkBorrowScope
//===----------------------------------------------------------------------===//

class ShrinkBorrowScope {
  // The instruction that begins this borrow scope.
  BeginBorrowInst *introducer;

  InstructionDeleter &deleter;

  SmallVectorImpl<CopyValueInst *> &modifiedCopyValueInsts;

  /// Instructions which are users of the simple (i.e. not reborrowed) extended
  /// i.e. copied lifetime of the introducer.
  SmallPtrSet<SILInstruction *, 16> users;

  /// Deinit barriers that obstruct hoisting end_borrow instructions.
  llvm::SmallVector<std::pair<SILBasicBlock *, SILInstruction *>>
      barrierInstructions;

  /// Blocks above which the borrow scope cannot be hoisted.
  ///
  /// Consequently, these blocks must begin with end_borrow %borrow.
  ///
  /// Note: These blocks aren't barrier blocks.  Rather the borrow scope is
  ///       barred from being hoisted out of them.  That could happen because
  ///       one of its predecessors is a barrier block (i.e. has a successor
  ///       which is live) or because one of its predecessors has a terminator
  ///       which is itself a deinit barrier.
  SmallPtrSet<SILBasicBlock *, 8> barredBlocks;

  llvm::SmallDenseMap<ApplySite, size_t> transitiveUsesPerApplySite;

  // The list of blocks to look for new points at which to insert end_borrows
  // in.  A block must not be processed if all of its successors have not yet
  // been.  For that reason, it is necessary to allow the same block to be
  // visited multiple times, at most once for each successor.
  SmallVector<SILBasicBlock *, 8> worklist;
  // The instructions from which the shrinking starts, the scope ending
  // instructions, keyed off the block in which they appear.
  llvm::SmallDenseMap<SILBasicBlock *, SILInstruction *> startingInstructions;
  // The end _borrow instructions for this borrow scope that existed before
  // ShrinkBorrowScope ran and which were not modified.
  llvm::SmallPtrSet<SILInstruction *, 8> reusedEndBorrowInsts;

  // Whether ShrinkBorrowScope made any changes to the function.
  //
  // It could have made one of the following sorts of changes:
  // - deleted an end_borrow
  // - created an end_borrow
  // - rewrote the operand of an instruction
  //   - ApplySite
  //   - begin_borrow
  //   - copy_value
  bool madeChange;

public:
  ShrinkBorrowScope(BeginBorrowInst *bbi, InstructionDeleter &deleter,
                    SmallVectorImpl<CopyValueInst *> &modifiedCopyValueInsts)
      : introducer(bbi), deleter(deleter),
        modifiedCopyValueInsts(modifiedCopyValueInsts), madeChange(false) {}

  bool run();

  bool populateUsers();
  bool initializeWorklist();
  void findBarriers();
  bool rewrite();
  bool createEndBorrow(SILInstruction *insertionPoint);

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

  bool canHoistOverInstruction(SILInstruction *instruction) {
    return tryHoistOverInstruction(instruction, /*rewrite=*/false);
  }

  bool tryHoistOverInstruction(SILInstruction *instruction, bool rewrite = true) {
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
        if (rewrite) {
          // We can rewrite all the arguments which are transitive uses of the
          // borrow.
          for (auto index : rewritableArgumentIndices) {
            auto argument = apply.getArgument(index);
            auto borrowee = introducer->getOperand();
            if (auto *cvi = dyn_cast<CopyValueInst>(argument)) {
              cvi->setOperand(borrowee);
              modifiedCopyValueInsts.push_back(cvi);
              madeChange = true;
            } else {
              apply.setArgument(index, borrowee);
              madeChange = true;
            }
          }
        }
        return true;
      } else if (auto *bbi = dyn_cast<BeginBorrowInst>(instruction)) {
        if (bbi->isLexical() &&
            canReplaceValueWithBorrowedValue(bbi->getOperand())) {
          if (rewrite) {
            auto borrowee = introducer->getOperand();
            bbi->setOperand(borrowee);
            madeChange = true;
          }
          return true;
        }
      } else if (auto *cvi = dyn_cast<CopyValueInst>(instruction)) {
        if (canReplaceValueWithBorrowedValue(cvi->getOperand())) {
          if (rewrite) {
            auto borrowee = introducer->getOperand();
            cvi->setOperand(borrowee);
            madeChange = true;
            modifiedCopyValueInsts.push_back(cvi);
          }
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

  madeChange |= rewrite();

  return madeChange;
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

  // Blocks to the top of which the borrow scope has been shrunk.
  SmallPtrSet<SILBasicBlock *, 8> deadBlocks;
  auto hasOnlyDeadSuccessors =
      [&deadBlocks](SILBasicBlock *block) -> bool {
    return llvm::all_of(block->getSuccessorBlocks(), [=](auto *successor) {
      return deadBlocks.contains(successor);
    });
  };

  while (!worklist.empty()) {
    auto *block = worklist.pop_back_val();
    auto *startingInstruction = startingInstructions.lookup(block);
    if (!startingInstruction && !hasOnlyDeadSuccessors(block)) {
      continue;
    }
    for (auto *successor : block->getSuccessorBlocks()) {
      barredBlocks.erase(successor);
    }

    // We either have processed all successors of block or else it is a block
    // which contained one of the original scope-ending instructions.  Scan the
    // block backwards, looking for the first deinit barrier.  If we've visited
    // all successors, start scanning from the terminator.  If the block
    // contained an original scope-ending instruction, start scanning from it.
    SILInstruction *instruction =
        startingInstruction ? startingInstruction : block->getTerminator();
    if (!startingInstruction) {
      // That there's no starting instruction means that this this block did not
      // contain an original introducer.  It was added to the worklist later.
      // At that time, it was checked that this block (along with all that
      // successor's other predecessors) had a terminator over which the borrow
      // scope could be shrunk.  Shrink it now.
      assert(tryHoistOverInstruction(block->getTerminator()));
    }
    SILInstruction *barrier = nullptr;
    while ((instruction = instruction->getPreviousInstruction())) {
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
      deadBlocks.insert(block);
      barredBlocks.insert(block);
      // If any of block's predecessor has a terminator over which the scope 
      // can't be shrunk, the scope is barred from shrinking out of this block.
      if (llvm::all_of(block->getPredecessorBlocks(), [&](auto *block) {
            return canHoistOverInstruction(block->getTerminator());
            })) {
        // Otherwise, add all predecessors to the worklist and attempt to shrink
        // the borrow scope through them.
        for (auto *predecessor : block->getPredecessorBlocks()) {
          worklist.push_back(predecessor);
        }
      }
    }
  }
}

bool ShrinkBorrowScope::rewrite() {
  bool createdBorrow = false;

  // Insert the new end_borrow instructions that occur after deinit barriers.
  for (auto pair : barrierInstructions) {
    auto *insertionPoint = pair.second->getNextInstruction();
    createdBorrow |= createEndBorrow(insertionPoint);
  }

  // Insert the new end_borrow instructions that occur at the beginning of
  // blocks which we couldn't hoist out of.
  for (auto *block : barredBlocks) {
    auto *insertionPoint = &*block->begin();
    createdBorrow |= createEndBorrow(insertionPoint);
  }

  if (createdBorrow) {
    // Remove all the original end_borrow instructions.
    for (auto pair : startingInstructions) {
      if (reusedEndBorrowInsts.contains(pair.second)) {
        continue;
      }
      deleter.forceDelete(pair.getSecond());
    }
  }

  return createdBorrow;
}

bool ShrinkBorrowScope::createEndBorrow(SILInstruction *insertionPoint) {
  if (auto *ebi = dyn_cast<EndBorrowInst>(insertionPoint)) {
    llvm::SmallDenseMap<SILBasicBlock *, SILInstruction *>::iterator location;
    if ((location = llvm::find_if(startingInstructions, [&](auto pair) -> bool {
           return pair.second == insertionPoint;
         })) != startingInstructions.end()) {
      reusedEndBorrowInsts.insert(location->second);
      return false;
    }
  }
  auto builder = SILBuilderWithScope(insertionPoint);
  builder.createEndBorrow(
      RegularLocation::getAutoGeneratedLocation(insertionPoint->getLoc()),
      introducer);
  return true;
}

bool swift::shrinkBorrowScope(
    BeginBorrowInst *bbi, InstructionDeleter &deleter,
    SmallVectorImpl<CopyValueInst *> &modifiedCopyValueInsts) {
  ShrinkBorrowScope borrowShrinker(bbi, deleter, modifiedCopyValueInsts);
  return borrowShrinker.run();
}
