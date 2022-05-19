//=====-- ShrinkBorrowScope.cpp - Hoist end_borrows to deinit barriers. -=====//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
/// Shrink borrow scopes by hoisting end_borrows up to deinit barriers.  After
/// this is done, CanonicalOSSALifetime is free to hoist the destroys of the
/// owned value up to the end_borrow.  In this way, the lexical lifetime of
/// guaranteed values is preserved.
//===----------------------------------------------------------------------===//

#include "swift/AST/Builtins.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/Reachability.h"
#include "swift/SILOptimizer/Utils/CanonicalizeBorrowScope.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "llvm/ADT/STLExtras.h"

#define DEBUG_TYPE "copy-propagation"

using namespace swift;

//===----------------------------------------------------------------------===//
//                       MARK: ShrinkBorrowScope
//===----------------------------------------------------------------------===//

namespace ShrinkBorrowScope {

/// The environment within which to hoist.
struct Context final {
  /// The instruction that begins the borrow scope.
  BeginBorrowInst const &introducer;

  /// BorrowedValue(introducer)
  BorrowedValue const borrowedValue;

  /// The value whose lifetime is guaranteed by the lexical borrow scope.
  ///
  /// introducer->getOperand()
  SILValue const borrowee;

  SILFunction &function;

  /// The copy_value instructions that the utility creates or changes.
  ///
  /// Clients provide this so that they can update worklists in respons.
  SmallVectorImpl<CopyValueInst *> &modifiedCopyValueInsts;

  InstructionDeleter &deleter;

  Context(BeginBorrowInst const &introducer,
          SmallVectorImpl<CopyValueInst *> &modifiedCopyValueInsts,
          InstructionDeleter &deleter)
      : introducer(introducer), borrowedValue(BorrowedValue(&introducer)),
        borrowee(introducer.getOperand()), function(*introducer.getFunction()),
        modifiedCopyValueInsts(modifiedCopyValueInsts), deleter(deleter) {}
  Context(Context const &) = delete;
  Context &operator=(Context const &) = delete;
};

/// How %lifetime gets used.
struct Usage final {
  /// Instructions which are users of the simple (i.e. not reborrowed) extended
  /// i.e. copied lifetime of the introducer.
  SmallPtrSet<SILInstruction *, 16> users;
  // The instructions from which the shrinking starts, the scope ending
  // instructions.
  llvm::SmallSetVector<SILInstruction *, 4> ends;

  Usage(){};
  Usage(Usage const &) = delete;
  Usage &operator=(Usage const &) = delete;
};

/// Identify scope ending uses and extended users of %lifetime.
///
/// returns true if all uses were found
///         false otherwise
bool findUsage(Context const &context, Usage &usage) {
  llvm::SmallVector<SILInstruction *, 16> scopeEndingInsts;
  context.borrowedValue.getLocalScopeEndingInstructions(scopeEndingInsts);

  // Add all the end_borrows to the collection of ends.
  for (auto *instruction : scopeEndingInsts) {
    // If a scope ending instruction is not an end_borrow, bail out.
    if (!isa<EndBorrowInst>(instruction))
      return false;
    usage.ends.insert(instruction);
  }

  SmallVector<Operand *, 16> uses;
  if (!findExtendedUsesOfSimpleBorrowedValue(context.borrowedValue, &uses)) {
    // If the value produced by begin_borrow escapes, don't shrink the borrow
    // scope.
    return false;
  }
  for (auto *use : uses) {
    usage.users.insert(use->getUser());
  }
  return true;
}

/// How end_borrow hoisting is obstructed.
struct DeinitBarriers final {
  /// Blocks up to "before the beginning" of which hoisting was able to proceed.
  BasicBlockSetVector hoistingReachesBeginBlocks;

  /// Blocks to "after the end" of which hoisting was able to proceed.
  BasicBlockSet hoistingReachesEndBlocks;

  /// Copies to be rewritten as copies of %borrowee.
  SmallVector<CopyValueInst *, 4> copies;

  /// Instructions above which end_borrows cannot be hoisted.
  SmallVector<SILInstruction *, 4> barriers;

  /// Blocks one of whose phis is a barrier and consequently out of which
  /// end_borrows cannot be hoisted.
  SmallVector<SILBasicBlock *, 4> phiBarriers;

  DeinitBarriers(Context &context)
      : hoistingReachesBeginBlocks(&context.function),
        hoistingReachesEndBlocks(&context.function) {}
  DeinitBarriers(DeinitBarriers const &) = delete;
  DeinitBarriers &operator=(DeinitBarriers const &) = delete;
};

/// Works backwards from the current location of end_borrows to the earliest
/// place they can be hoisted to.
///
/// Implements BackwardReachability::BlockReachability.
class Dataflow final {
  Context const &context;
  Usage const &uses;
  DeinitBarriers &result;

  enum class Classification { Barrier, Copy, Other };

  BackwardReachability<Dataflow> reachability;

public:
  Dataflow(Context const &context, Usage const &uses, DeinitBarriers &result)
      : context(context), uses(uses), result(result),
        reachability(&context.function, *this) {
    // Seed reachability with the scope ending uses from which the backwards
    // data flow will begin.
    for (auto *end : uses.ends) {
      reachability.initLastUse(end);
    }
  }
  Dataflow(Dataflow const &) = delete;
  Dataflow &operator=(Dataflow const &) = delete;

  void run() { reachability.solveBackward(); }

private:
  friend class BackwardReachability<Dataflow>;

  bool hasReachableBegin(SILBasicBlock *block) {
    return result.hoistingReachesBeginBlocks.contains(block);
  }

  void markReachableBegin(SILBasicBlock *block) {
    result.hoistingReachesBeginBlocks.insert(block);
  }

  void markReachableEnd(SILBasicBlock *block) {
    result.hoistingReachesEndBlocks.insert(block);
  }

  Classification classifyInstruction(SILInstruction *);

  bool classificationIsBarrier(Classification);

  void visitedInstruction(SILInstruction *, Classification);

  bool checkReachableBarrier(SILInstruction *);

  bool checkReachablePhiBarrier(SILBasicBlock *);
};

/// Whether the specified value is %lifetime or its iterated copy_value.
///
/// In other words, it has to be a simple extended def of %lifetime.
bool isSimpleExtendedIntroducerDef(Context const &context, SILValue value) {
  while (true) {
    auto *instruction = value.getDefiningInstruction();
    if (!instruction)
      return false;
    if (instruction == &context.introducer)
      return true;
    if (auto *cvi = dyn_cast<CopyValueInst>(instruction)) {
      value = cvi->getOperand();
      continue;
    }
    return false;
  }
}

Dataflow::Classification
Dataflow::classifyInstruction(SILInstruction *instruction) {
  if (instruction == &context.introducer) {
    return Classification::Barrier;
  }
  if (auto *cvi = dyn_cast<CopyValueInst>(instruction)) {
    if (isSimpleExtendedIntroducerDef(context, cvi->getOperand())) {
      return Classification::Copy;
    }
  }
  if (uses.users.contains(instruction)) {
    return Classification::Barrier;
  }
  if (isDeinitBarrier(instruction)) {
    return Classification::Barrier;
  }
  return Classification::Other;
}

bool Dataflow::classificationIsBarrier(Classification classification) {
  switch (classification) {
  case Classification::Barrier:
    return true;
  case Classification::Copy:
  case Classification::Other:
    return false;
  }
  llvm_unreachable("exhaustive switch not exhaustive?!");
}

void Dataflow::visitedInstruction(SILInstruction *instruction,
                                  Classification classification) {
  assert(classifyInstruction(instruction) == classification);
  switch (classification) {
  case Classification::Barrier:
    result.barriers.push_back(instruction);
    return;
  case Classification::Copy:
    result.copies.push_back(cast<CopyValueInst>(instruction));
    return;
  case Classification::Other:
    return;
  }
  llvm_unreachable("exhaustive switch not exhaustive?!");
}

bool Dataflow::checkReachableBarrier(SILInstruction *instruction) {
  auto classification = classifyInstruction(instruction);
  visitedInstruction(instruction, classification);
  return classificationIsBarrier(classification);
}

bool Dataflow::checkReachablePhiBarrier(SILBasicBlock *block) {
  assert(llvm::all_of(block->getArguments(),
                      [&](auto argument) { return PhiValue(argument); }));

  bool isBarrier =
      llvm::any_of(block->getPredecessorBlocks(), [&](auto *predecessor) {
        return classificationIsBarrier(
            classifyInstruction(predecessor->getTerminator()));
      });
  if (isBarrier) {
    result.phiBarriers.push_back(block);
  }
  return isBarrier;
}

/// Hoist the scope ends of %lifetime, rewriting copies and borrows along the
/// way.
class Rewriter final {
  Context &context;
  Usage const &uses;
  DeinitBarriers const &barriers;

  // The end _borrow instructions for this borrow scope that existed before
  // ShrinkBorrowScope ran and which were not modified.
  llvm::SmallPtrSet<SILInstruction *, 8> reusedEndBorrowInsts;

public:
  Rewriter(Context &context, Usage const &uses, DeinitBarriers const &barriers)
      : context(context), uses(uses), barriers(barriers) {}
  Rewriter(Rewriter const &) = delete;
  Rewriter &operator=(Rewriter const &) = delete;

  bool run();

private:
  bool createEndBorrow(SILInstruction *insertionPoint);
};

bool Rewriter::run() {
  bool madeChange = false;

  for (auto *cvi : barriers.copies) {
    cvi->setOperand(context.borrowee);
    context.modifiedCopyValueInsts.push_back(cvi);
    madeChange = true;
  }

  // Add end_borrows for phi barrier boundaries.
  //
  // A block is a phi barrier iff any of its predecessors' terminators get
  // classified as barriers.  That happens when a copy of %lifetime is passed
  // to a phi.
  for (auto *block : barriers.phiBarriers) {
    madeChange |= createEndBorrow(&block->front());
  }

  // Add end_borrows for barrier boundaries.
  //
  // Insert end_borrows after every non-terminator barrier.
  //
  // For terminator barriers, add end_borrows at the beginning of the successor
  // blocks.  In order to reach a terminator and classify it as a barrier, all
  // of a block P's successors B had reachable beginnings.  If any of them
  // didn't, then BackwardReachability::meetOverSuccessors would never have
  // returned true for P, so none of its instructions would ever have been
  // classified (except for via checkReachablePhiBarrier, which doesn't record
  // terminator barriers).
  for (auto instruction : barriers.barriers) {
    if (auto *terminator = dyn_cast<TermInst>(instruction)) {
      auto successors = terminator->getParentBlock()->getSuccessorBlocks();
      // In order for the instruction to have been classified as a barrier,
      // reachability would have had to reach the block containing it.
      assert(barriers.hoistingReachesEndBlocks.contains(
                terminator->getParentBlock()));
      for (auto *successor : successors) {
        madeChange |= createEndBorrow(&successor->front());
      }
    } else {
      auto *next = instruction->getNextInstruction();
      assert(next);
      madeChange |= createEndBorrow(next);
    }
  }

  // Add end_borrows for control-flow boundaries.
  //
  // Insert end_borrows at the beginning of blocks which were preceded by a
  // control flow branch (and which, thanks to the lack of critical edges,
  // don't have multiple predecessors) whose end was not reachable (because
  // reachability was not able to make it to the top of some other successor).
  //
  // In other words, a control flow boundary is the target edge from a block B
  // to its single predecessor P not all of whose successors S in succ(P) had
  // reachable beginnings.  We witness that fact about P's successors by way of
  // P not having a reachable end--see BackwardReachability::meetOverSuccessors.
  //
  // control-flow-boundary(B) := beginning-reachable(B) && !end-reachable(P)
  for (auto *block : barriers.hoistingReachesBeginBlocks) {
    if (auto *predecessor = block->getSinglePredecessorBlock()) {
      if (!barriers.hoistingReachesEndBlocks.contains(predecessor)) {
        madeChange |= createEndBorrow(&block->front());
      }
    }
  }

  if (madeChange) {
    // Remove all the original end_borrow instructions.
    for (auto *end : uses.ends) {
      if (reusedEndBorrowInsts.contains(end)) {
        continue;
      }
      context.deleter.forceDelete(end);
    }
  }

  return madeChange;
}

bool Rewriter::createEndBorrow(SILInstruction *insertionPoint) {
  if (auto *ebi = dyn_cast<EndBorrowInst>(insertionPoint)) {
    if (uses.ends.contains(insertionPoint)) {
      reusedEndBorrowInsts.insert(insertionPoint);
      return false;
    }
  }
  auto builder = SILBuilderWithScope(insertionPoint);
  builder.createEndBorrow(
      RegularLocation::getAutoGeneratedLocation(insertionPoint->getLoc()),
      &context.introducer);
  return true;
}

bool run(Context &context) {
  Usage usage;
  if (!findUsage(context, usage))
    return false;

  DeinitBarriers barriers(context);
  Dataflow flow(context, usage, barriers);
  flow.run();

  Rewriter rewriter(context, usage, barriers);

  return rewriter.run();
}
} // end namespace ShrinkBorrowScope

bool swift::shrinkBorrowScope(
    BeginBorrowInst const &bbi, InstructionDeleter &deleter,
    SmallVectorImpl<CopyValueInst *> &modifiedCopyValueInsts) {
  ShrinkBorrowScope::Context context(bbi, modifiedCopyValueInsts, deleter);
  return ShrinkBorrowScope::run(context);
}
