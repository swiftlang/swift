//=-- LexicalDestroyHoisting.cpp - Hoist destroy_values to deinit barriers. -=//
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
/// Hoist destroys of owned lexical values (owned arguments and the results of
/// move_value [lexical] instructions) up to deinit barriers.
//===----------------------------------------------------------------------===//

#include "swift/AST/Builtins.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/Reachability.h"
#include "swift/SILOptimizer/Utils/CanonicalizeBorrowScope.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "llvm/ADT/STLExtras.h"

#define DEBUG_TYPE "copy-propagation"

using namespace swift;

//===----------------------------------------------------------------------===//
//                       MARK: LexicalDestroyHoisting
//===----------------------------------------------------------------------===//

namespace LexicalDestroyHoisting {

/// The environment within which to hoist.
struct Context final {
  /// The owned lexical value whose destroys are to be hoisted.
  SILValue const &value;

  /// value->getDefiningInstruction()
  SILInstruction *const definition;

  SILFunction &function;

  InstructionDeleter &deleter;

  Context(SILValue const &value, SILFunction &function,
          InstructionDeleter &deleter)
      : value(value), definition(value->getDefiningInstruction()),
        function(function), deleter(deleter) {
    assert(value->isLexical());
    assert(value->getOwnershipKind() == OwnershipKind::Owned);
  }
  Context(Context const &) = delete;
  Context &operator=(Context const &) = delete;
};

/// How %value gets used.
struct Usage final {
  /// Instructions which are users of the simple (i.e. not reborrowed) value.
  SmallPtrSet<SILInstruction *, 16> users;
  // The instructions from which the hoisting starts, the destroy_values.
  llvm::SmallSetVector<SILInstruction *, 4> ends;

  Usage(){};
  Usage(Usage const &) = delete;
  Usage &operator=(Usage const &) = delete;
};

/// Identify users and destroy_values of %value.
///
/// returns true if all uses were found
///         false otherwise
bool findUsage(Context const &context, Usage &usage) {
  SmallVector<Operand *, 16> uses;
  if (!findUsesOfSimpleValue(context.value, &uses)) {
    // If the value escapes, don't hoist.
    return false;
  }
  for (auto *use : uses) {
    // Add the destroy_values to the collection of ends so we can seed the data
    // flow and determine whether any were reused.  They aren't uses over which
    // we can't hoist though.
    if (isa<DestroyValueInst>(use->getUser())) {
      usage.ends.insert(use->getUser());
    } else {
      usage.users.insert(use->getUser());
    }
  }
  return true;
}

/// How destroy_value hoisting is obstructed.
struct DeinitBarriers final {
  /// Blocks up to "before the beginning" of which hoisting was able to proceed.
  BasicBlockSetVector hoistingReachesBeginBlocks;

  /// Blocks to "after the end" of which hoisting was able to proceed.
  BasicBlockSet hoistingReachesEndBlocks;

  /// Instructions above which destroy_values cannot be hoisted.
  SmallVector<SILInstruction *, 4> barriers;

  /// Blocks one of whose phis is a barrier and consequently out of which
  /// destroy_values cannot be hoisted.
  SmallVector<SILBasicBlock *, 4> phiBarriers;

  DeinitBarriers(Context &context)
      : hoistingReachesBeginBlocks(&context.function),
        hoistingReachesEndBlocks(&context.function) {}
  DeinitBarriers(DeinitBarriers const &) = delete;
  DeinitBarriers &operator=(DeinitBarriers const &) = delete;
};

/// Works backwards from the current location of destroy_values to the earliest
/// place they can be hoisted to.
///
/// Implements BackwardReachability::BlockReachability.
class DataFlow final {
  Context const &context;
  Usage const &uses;
  DeinitBarriers &result;

  enum class Classification { Barrier, Other };

  BackwardReachability<DataFlow> reachability;

public:
  DataFlow(Context const &context, Usage const &uses, DeinitBarriers &result)
      : context(context), uses(uses), result(result),
        reachability(&context.function, *this) {
    // Seed reachability with the scope ending uses from which the backwards
    // data flow will begin.
    for (auto *end : uses.ends) {
      reachability.initLastUse(end);
    }
  }
  DataFlow(DataFlow const &) = delete;
  DataFlow &operator=(DataFlow const &) = delete;

  void run() { reachability.solveBackward(); }

private:
  friend class BackwardReachability<DataFlow>;

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

DataFlow::Classification
DataFlow::classifyInstruction(SILInstruction *instruction) {
  if (instruction == context.definition) {
    return Classification::Barrier;
  }
  if (uses.users.contains(instruction)) {
    return Classification::Barrier;
  }
  if (isDeinitBarrier(instruction)) {
    return Classification::Barrier;
  }
  return Classification::Other;
}

bool DataFlow::classificationIsBarrier(Classification classification) {
  switch (classification) {
  case Classification::Barrier:
    return true;
  case Classification::Other:
    return false;
  }
  llvm_unreachable("exhaustive switch not exhaustive?!");
}

void DataFlow::visitedInstruction(SILInstruction *instruction,
                                  Classification classification) {
  assert(classifyInstruction(instruction) == classification);
  switch (classification) {
  case Classification::Barrier:
    result.barriers.push_back(instruction);
    return;
  case Classification::Other:
    return;
  }
  llvm_unreachable("exhaustive switch not exhaustive?!");
}

bool DataFlow::checkReachableBarrier(SILInstruction *instruction) {
  auto classification = classifyInstruction(instruction);
  visitedInstruction(instruction, classification);
  return classificationIsBarrier(classification);
}

bool DataFlow::checkReachablePhiBarrier(SILBasicBlock *block) {
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

/// Hoist the destroy_values of %value.
class Rewriter final {
  Context &context;
  Usage const &uses;
  DeinitBarriers const &barriers;

  /// The destroy_value instructions for this owned lexical value that existed
  /// before LexicalDestroyHoisting ran and which were not modified.
  llvm::SmallPtrSet<SILInstruction *, 8> reusedDestroyValueInsts;

public:
  Rewriter(Context &context, Usage const &uses, DeinitBarriers const &barriers)
      : context(context), uses(uses), barriers(barriers) {}
  Rewriter(Rewriter const &) = delete;
  Rewriter &operator=(Rewriter const &) = delete;

  bool run();

private:
  bool createDestroyValue(SILInstruction *insertionPoint);
};

bool Rewriter::run() {
  bool madeChange = false;

  // Add destroy_values for phi barrier boundaries.
  //
  // A block is a phi barrier iff any of its predecessors' terminators get
  // classified as barriers.
  for (auto *block : barriers.phiBarriers) {
    madeChange |= createDestroyValue(&block->front());
  }

  // Add destroy_values for barrier boundaries.
  //
  // Insert destroy_values after every non-terminator barrier.
  //
  // For terminator barriers, add destroy_values at the beginning of the
  // successor blocks.  In order to reach a terminator and classify it as a
  // barrier, all of a block P's successors B had reachable beginnings.  If any
  // of them didn't, then BackwardReachability::meetOverSuccessors would never
  // have returned true for P, so none of its instructions would ever have been
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
        madeChange |= createDestroyValue(&successor->front());
      }
    } else {
      auto *next = instruction->getNextInstruction();
      assert(next);
      madeChange |= createDestroyValue(next);
    }
  }

  // Add destroy_values for control-flow boundaries.
  //
  // Insert destroy_values at the beginning of blocks which were preceded by a
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
        madeChange |= createDestroyValue(&block->front());
      }
    }
  }

  if (madeChange) {
    // Remove all the original destroy_values instructions.
    for (auto *end : uses.ends) {
      if (reusedDestroyValueInsts.contains(end)) {
        continue;
      }
      context.deleter.forceDelete(end);
    }
  }

  return madeChange;
}

bool Rewriter::createDestroyValue(SILInstruction *insertionPoint) {
  if (auto *ebi = dyn_cast<DestroyValueInst>(insertionPoint)) {
    if (uses.ends.contains(insertionPoint)) {
      reusedDestroyValueInsts.insert(insertionPoint);
      return false;
    }
  }
  auto builder = SILBuilderWithScope(insertionPoint);
  builder.createDestroyValue(
      RegularLocation::getAutoGeneratedLocation(insertionPoint->getLoc()),
      context.value);
  return true;
}

bool run(Context &context) {
  Usage usage;
  if (!findUsage(context, usage))
    return false;

  DeinitBarriers barriers(context);
  DataFlow flow(context, usage, barriers);
  flow.run();

  Rewriter rewriter(context, usage, barriers);

  return rewriter.run();
}
} // end namespace LexicalDestroyHoisting

bool swift::hoistDestroysOfOwnedLexicalValue(SILValue const value,
                                             SILFunction &function,
                                             InstructionDeleter &deleter) {
  if (!value->isLexical())
    return false;
  if (value->getOwnershipKind() != OwnershipKind::Owned)
    return false;
  LexicalDestroyHoisting::Context context(value, function, deleter);
  return LexicalDestroyHoisting::run(context);
}
