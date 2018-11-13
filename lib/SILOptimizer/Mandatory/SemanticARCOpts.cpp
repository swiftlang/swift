//===--- SemanticARCOpts.cpp ----------------------------------------------===//
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

#define DEBUG_TYPE "sil-semantic-arc-opts"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;

STATISTIC(NumEliminatedInsts, "number of removed instructions");

namespace {

struct SemanticARCOptVisitor
    : SILInstructionVisitor<SemanticARCOptVisitor, bool> {
  bool visitSILInstruction(SILInstruction *i) { return false; }
  bool visitCopyValueInst(CopyValueInst *cvi);
  bool visitBeginBorrowInst(BeginBorrowInst *bbi);
};

} // end anonymous namespace

bool SemanticARCOptVisitor::visitBeginBorrowInst(BeginBorrowInst *bbi) {
  auto kind = bbi->getOperand().getOwnershipKind();
  SmallVector<EndBorrowInst *, 16> endBorrows;
  for (auto *op : bbi->getUses()) {
    auto *user = op->getUser();
    switch (user->getKind()) {
    case SILInstructionKind::EndBorrowInst:
      endBorrows.push_back(cast<EndBorrowInst>(user));
      break;
    default:
      // Make sure that this operand can accept our arguments kind.
      auto map = op->getOwnershipKindMap();
      if (map.canAcceptKind(kind))
        continue;
      return false;
    }
  }

  // At this point, we know that the begin_borrow's operand can be
  // used as an argument to all non-end borrow uses. Eliminate the
  // begin borrow and end borrows.
  while (!endBorrows.empty()) {
    auto *ebi = endBorrows.pop_back_val();
    ebi->eraseFromParent();
    ++NumEliminatedInsts;
  }
  bbi->replaceAllUsesWith(bbi->getOperand());
  bbi->eraseFromParent();
  ++NumEliminatedInsts;
  return true;
}

static bool canHandleOperand(SILValue operand, SmallVectorImpl<SILValue> &out) {
  if (!getUnderlyingBorrowIntroducers(operand, out))
    return false;

  /// TODO: Add support for begin_borrow, load_borrow.
  return all_of(out, [](SILValue v) { return isa<SILFunctionArgument>(v); });
}

static bool performGuaranteedCopyValueOptimization(CopyValueInst *cvi) {
  SmallVector<SILValue, 16> borrowIntroducers;

  // Whitelist the operands that we know how to support and make sure
  // our operand is actually guaranteed.
  if (!canHandleOperand(cvi->getOperand(), borrowIntroducers))
    return false;

  // Then go over all of our uses. Find our destroying instructions
  // and make sure all of them are destroy_value. For our
  // non-destroying instructions, make sure that they accept a
  // guaranteed value. After that, make sure that our destroys are
  // within the lifetime of our borrowed values.
  SmallVector<DestroyValueInst *, 16> destroys;
  for (auto *op : cvi->getUses()) {
    // We know that a copy_value produces an @owned value. Look
    // through all of our uses and classify them as either
    // invalidating or not invalidating. Make sure that all of the
    // invalidating ones are destroy_value since otherwise the
    // live_range is not complete.
    auto map = op->getOwnershipKindMap();
    auto constraint = map.getLifetimeConstraint(ValueOwnershipKind::Owned);
    switch (constraint) {
    case UseLifetimeConstraint::MustBeInvalidated:
      // And we have a destroy_value, track it and continue.
      if (auto *dvi = dyn_cast<DestroyValueInst>(op->getUser())) {
        destroys.push_back(dvi);
        continue;
      }
      // Otherwise, we found a non-destroy value invalidating owned
      // user... This is not an unnecessary live range.
      return false;
    case UseLifetimeConstraint::MustBeLive:
      // Ok, this constraint can take something owned as live. Lets
      // see if it can also take something that is guaranteed. If it
      // can not, then we bail.
      if (!map.canAcceptKind(ValueOwnershipKind::Guaranteed)) {
        return false;
      }

      // Otherwise, continue.
      continue;
    }
  }

  // If we reached this point, then we know that all of our users can
  // accept a guaranteed value and our owned value is destroyed only
  // by destroy_value. Check if all of our destroys are joint
  // post-dominated by the end_borrow set. If they do not, then the
  // copy_value is lifetime extending the guaranteed value, we can not
  // eliminate it.
  //
  // TODO: When we support begin_borrow/load_borrow a linear linfetime
  // check will be needed here.
  assert(all_of(borrowIntroducers,
                [](SILValue v) { return isa<SILFunctionArgument>(v); }));

  // Otherwise, we know that our copy_value/destroy_values are all
  // completely within the guaranteed value scope.
  while (!destroys.empty()) {
    auto *dvi = destroys.pop_back_val();
    dvi->eraseFromParent();
    ++NumEliminatedInsts;
  }
  cvi->replaceAllUsesWith(cvi->getOperand());
  cvi->eraseFromParent();
  ++NumEliminatedInsts;
  return true;
}

bool SemanticARCOptVisitor::visitCopyValueInst(CopyValueInst *cvi) {
  // If our copy value inst has a single destroy value user, eliminate
  // it.
  if (auto *op = cvi->getSingleUse()) {
    if (auto *dvi = dyn_cast<DestroyValueInst>(op->getUser())) {
      dvi->eraseFromParent();
      cvi->eraseFromParent();
      NumEliminatedInsts += 2;
      return true;
    }
  }

  // Then try to perform the guaranteed copy value optimization.
  if (performGuaranteedCopyValueOptimization(cvi))
    return true;

  return false;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

// Even though this is a mandatory pass, it is rerun after deserialization in
// case DiagnosticConstantPropagation exposed anything new in this assert
// configuration.
struct SemanticARCOpts : SILFunctionTransform {
  void run() override {
    SILFunction &f = *getFunction();
    // Do not run the semantic arc opts unless we are running on the
    // standard library. This is because this is the only module that
    // passes the ownership verifier.
    if (!f.getModule().isStdlibModule())
      return;

    // Iterate over all of the arguments, performing small peephole
    // ARC optimizations.
    //
    // FIXME: Should we iterate or use a RPOT order here?
    bool madeChange = false;
    for (auto &bb : f) {
      auto ii = bb.rend();
      auto start = bb.rbegin();

      // If the bb is empty, continue.
      if (start == ii)
        continue;

      // Go to the first instruction to process.
      --ii;

      // Then until we process the first instruction of the block...
      while (ii != start) {
        // Move the iterator before ii.
        auto tmp = std::next(ii);

        // Then try to optimize. If we succeeded, then we deleted
        // ii. Move ii from the next value back onto the instruction
        // after ii's old value in the block instruction list and then
        // process that.
        if (SemanticARCOptVisitor().visit(&*ii)) {
          madeChange = true;
          ii = std::prev(tmp);
          continue;
        }

        // Otherwise, we didn't delete ii. Just visit the next instruction.
        --ii;
      }

      // Finally visit the first instruction of the block.
      madeChange |= SemanticARCOptVisitor().visit(&*ii);
    }

    if (madeChange) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createSemanticARCOpts() { return new SemanticARCOpts(); }
