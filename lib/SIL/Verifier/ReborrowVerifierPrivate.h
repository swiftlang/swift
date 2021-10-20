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
  DeadEndBlocks *deadEndBlocks = nullptr;
  /// The builder that the checker uses to emit error messages, crash if asked
  /// for, or supply back interesting info to the caller.
  LinearLifetimeChecker::ErrorBuilder errorBuilder;
  /// A map of reborrow phi arg to its base values.
  /// Note that a reborrow phi arg can have different base values based on
  /// different control flow paths.
  llvm::DenseMap<SILPhiArgument *, SmallPtrSet<SILValue, 8>>
      reborrowToBaseValuesMap;

public:
  ReborrowVerifier(const SILFunction *func, DeadEndBlocks *deadEndBlocks,
                   LinearLifetimeChecker::ErrorBuilder errorBuilder)
      : deadEndBlocks(deadEndBlocks), errorBuilder(errorBuilder) {}

  /// Find all the reborrows of \p initialScopedOperand and verify their
  /// lifetime.
  void verifyReborrows(BorrowingOperand initialScopedOperand, SILValue value);

private:
  /// Verifies whether the reborrow's lifetime lies within its base value
  bool verifyReborrowLifetime(SILPhiArgument *phiArg, SILValue baseVal);
};

} // namespace swift

#endif
