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
  bool result = false;
  SmallVector<Operand *, 4> phiArgUses(phiArg->getUses());

  // Verify whether the guaranteed phi arg lies within the lifetime of the base
  // value.
  LinearLifetimeChecker checker(deadEndBlocks);
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
  auto visitReborrowBaseValuePair = [&](SILPhiArgument *phiArg,
                                        SILValue baseValue) {
    // If the (phiArg, baseValue) pair was not visited before, verify the
    // lifetime.
    auto it = reborrowToBaseValuesMap.find(phiArg);
    if (it == reborrowToBaseValuesMap.end() ||
        it->second.find(baseValue) == it->second.end()) {
      reborrowToBaseValuesMap[phiArg].insert(baseValue);
      verifyReborrowLifetime(phiArg, baseValue);
    }
  };
  // For every unique reborrow/base value pair, verify the lifetime
  findTransitiveReborrowBaseValuePairs(initialScopedOperand, value,
                                       visitReborrowBaseValuePair);
}
