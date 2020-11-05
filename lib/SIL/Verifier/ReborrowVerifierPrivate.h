//===--- ReborrowVerifierPrivate.h ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_REBORROWVERIFIER_H
#define SWIFT_SIL_REBORROWVERIFIER_H

#include "LinearLifetimeCheckerPrivate.h"

#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"

namespace swift {

class DeadEndBlocks;

/// A guaranteed phi arg ends the borrow scope of its incoming value and begins
/// a new borrow scope. ReborrowVerifier validates the lifetime of the reborrow
/// lies within the lifetime of its base value. It uses LinearLifetimeChecker
/// for this.
class ReborrowVerifier {
  /// A cache of dead-end basic blocks that we use to determine if we can
  /// ignore "leaks".
  DeadEndBlocks &deadEndBlocks;
  /// A cache map of borrow lifetime ending operands and their base value
  llvm::SmallDenseMap<Operand *, SILValue> visitedOps;
  /// A cache map of guaranteed phi args and their base value
  llvm::SmallDenseMap<SILPhiArgument *, SILValue> visitedPhiArgs;
  /// The builder that the checker uses to emit error messages, crash if asked
  /// for, or supply back interesting info to the caller.
  LinearLifetimeChecker::ErrorBuilder errorBuilder;

public:
  ReborrowVerifier(const SILFunction *func, DeadEndBlocks &deadEndBlocks,
                   LinearLifetimeChecker::ErrorBuilder errorBuilder)
      : deadEndBlocks(deadEndBlocks), errorBuilder(errorBuilder) {}

  void verifyReborrows(BorrowingOperand initialScopedOperand, SILValue value);

private:
  /// Verifies whether the reborrow's lifetime lies within its base value
  bool verifyReborrowLifetime(SILPhiArgument *phiArg, SILValue baseVal);

  /// Check if the operand is visited
  bool isVisitedOp(Operand *op, SILValue baseVal) {
    return visitedOps.find(op) != visitedOps.end();
  }

  /// Check if the phi arg and base value are visited
  bool isVisitedPhiArg(SILPhiArgument *phiArg, SILValue baseVal) {
    auto itPhiArg = visitedPhiArgs.find(phiArg);
    return itPhiArg != visitedPhiArgs.end() && (*itPhiArg).second == baseVal;
  }

  /// Mark operand as visited
  void addVisitedOp(Operand *op, SILValue baseVal) {
    visitedOps.insert({op, baseVal});
  }

  /// Mark guaranteed phi arg as visited
  void addVisitedPhiArg(SILPhiArgument *phiArg, SILValue baseVal) {
    visitedPhiArgs.insert({phiArg, baseVal});
  }
};

} // namespace swift

#endif
