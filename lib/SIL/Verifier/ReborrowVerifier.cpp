//===--- ReborrowVerifier.cpp ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-reborrow-checker"

#include "ReborrowVerifierPrivate.h"

using namespace swift;

bool ReborrowVerifier::verifyReborrowLifetime(SILPhiArgument *phiArg,
                                              SILValue baseVal) {
  SmallPtrSet<SILBasicBlock *, 4> visitedBlocks;
  bool result = false;
  SmallVector<Operand *, 4> phiArgUses(phiArg->getUses());

  // Verify whether the guaranteed phi arg lies within the lifetime of the base
  // value.
  LinearLifetimeChecker checker(visitedBlocks, deadEndBlocks);
  // newErrorBuilder is consumed at the end of the checkValue function.
  // Copy initial state from errorBuilder everytime
  LinearLifetimeChecker::ErrorBuilder newErrorBuilder = errorBuilder;
  SmallVector<Operand *, 4> baseValConsumingUses(baseVal->getConsumingUses());
  // If the baseValue has no consuming uses, there is nothing more to verify
  if (baseValConsumingUses.empty())
    return false;
  auto linearLifetimeResult = checker.checkValue(baseVal, baseValConsumingUses,
                                                 phiArgUses, newErrorBuilder);
  result |= linearLifetimeResult.getFoundError();
  return result;
}

void ReborrowVerifier::verifyReborrows(BorrowingOperand initialScopedOperand,
                                       SILValue value) {
  SmallVector<std::tuple<Operand *, SILValue>, 4> worklist;
  // Initialize the worklist with borrow lifetime ending uses
  initialScopedOperand.visitLocalEndScopeUses([&](Operand *op) {
    worklist.emplace_back(op, value);
    return true;
  });

  while (!worklist.empty()) {
    Operand *borrowLifetimeEndOp;
    SILValue baseVal;
    std::tie(borrowLifetimeEndOp, baseVal) = worklist.pop_back_val();
    auto *borrowLifetimeEndUser = borrowLifetimeEndOp->getUser();

    auto borrowingOperand = BorrowingOperand::get(borrowLifetimeEndOp);
    if (!borrowingOperand || !borrowingOperand->isReborrow())
      continue;

    if (isVisitedOp(borrowLifetimeEndOp, baseVal))
      continue;

    // Process reborrow
    auto *branchInst = cast<BranchInst>(borrowLifetimeEndUser);
    for (auto *succBlock : branchInst->getSuccessorBlocks()) {
      auto *phiArg = cast<SILPhiArgument>(
          succBlock->getArgument(borrowLifetimeEndOp->getOperandNumber()));
      assert(phiArg->getOwnershipKind() == OwnershipKind::Guaranteed);

      SILValue newBaseVal = baseVal;
      // If the previous base value was also passed as a phi arg, that will be
      // the new base value.
      for (auto *arg : succBlock->getArguments()) {
        if (arg->getIncomingPhiValue(branchInst->getParent()) == baseVal) {
          newBaseVal = arg;
          break;
        }
      }

      if (isVisitedPhiArg(phiArg, newBaseVal))
        continue;
      addVisitedPhiArg(phiArg, newBaseVal);
      verifyReborrowLifetime(phiArg, newBaseVal);

      // Find the scope ending uses of the guaranteed phi arg and add it to the
      // worklist.
      auto scopedValue = BorrowedValue::get(phiArg);
      assert(scopedValue.hasValue());
      scopedValue->visitLocalScopeEndingUses([&](Operand *op) {
        addVisitedOp(op, newBaseVal);
        worklist.emplace_back(op, newBaseVal);
      });
    }
  }
}
