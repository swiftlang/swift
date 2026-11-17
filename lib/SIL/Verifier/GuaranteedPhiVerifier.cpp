//===--- GuaranteedPhiVerifier.cpp ----------------------------------------===//
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

#define DEBUG_TYPE "sil-guaranteed-phi-verifier"

#include "GuaranteedPhiVerifierPrivate.h"

using namespace swift;

bool GuaranteedPhiVerifier::verifyDependentPhiLifetime(SILPhiArgument *phi,
                                                       SILValue baseValue) {
  // If the (phi, baseValue) pair was visited before, return.
  auto it = dependentPhiToBaseValueMap.find(phi);
  if (it != dependentPhiToBaseValueMap.end() &&
      it->second.find(baseValue) != it->second.end()) {
    return false;
  }
  // Insert (phi, baseValue) into the map.
  dependentPhiToBaseValueMap[phi].insert(baseValue);

  // Now, verify whether phi's lifetime is within baseValue's lifetime
  SmallVector<Operand *, 4> baseValConsumingUses(lookThroughBorrowedFromUser(baseValue)->getConsumingUses());
  // If the baseValue has no consuming uses, there is nothing more to verify
  if (baseValConsumingUses.empty())
    return false;

  SmallVector<Operand *, 4> phiUses(lookThroughBorrowedFromUser(phi)->getUses());
  LinearLifetimeChecker checker;
  // Verify whether the guaranteed phi lies within the lifetime of the base
  // value.
  return !checker.checkValue(baseValue, baseValConsumingUses, phiUses,
                             /*printError=*/true);
}

void GuaranteedPhiVerifier::verifyGuaranteedForwardingPhis(
    BorrowedValue borrow) {
  auto visitGuaranteedPhiBaseValuePair = [&](SILPhiArgument *phi,
                                             SILValue baseValue) {
    verifyDependentPhiLifetime(phi, baseValue);
  };
  visitExtendedGuaranteedForwardingPhiBaseValuePairs(
      borrow, visitGuaranteedPhiBaseValuePair);
}
