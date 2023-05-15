//===--- OwnershipLifetimeCompletion.h ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// OSSA lifetime completion adds lifetime ending instructions to make
/// linear lifetimes complete.
///
/// Completion is bottom-up recursive over nested borrow scopes. Additionally,
/// this may be extended to support dependent owned lifetimes in the future to
/// handle owned non-escaping values.
///
/// Lexical lifetimes can only be incomplete as a result of dead-end blocks. In
/// this case, their lifetime ends immediately before the dead-end block.
///
/// Nonlexical lifetimes can be incomplete for any reason. Their lifetime ends
/// at the liveness boundary.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_OSSSALIFETIMECOMPLETION_H
#define SWIFT_SILOPTIMIZER_UTILS_OSSSALIFETIMECOMPLETION_H

#include "swift/SIL/NodeDatastructures.h"
#include "swift/SIL/OwnershipLiveness.h"
#include "swift/SIL/SILFunction.h"

namespace swift {

enum class LifetimeCompletion { NoLifetime, AlreadyComplete, WasCompleted };

class OSSALifetimeCompletion {
  // If domInfo is nullptr, then InteriorLiveness never assumes dominance. As a
  // result it may report extra unenclosedPhis. In that case, any attempt to
  // create a new phi would result in an immediately redundant phi.
  const DominanceInfo *domInfo = nullptr;

  // Cache intructions already handled by the recursive algorithm to avoid
  // recomputing their lifetimes.
  ValueSet completedValues;

public:
  OSSALifetimeCompletion(SILFunction *function, const DominanceInfo *domInfo)
    : domInfo(domInfo), completedValues(function) {}

  /// Insert a lifetime-ending instruction on every path to complete the OSSA
  /// lifetime of \p value. Lifetime completion is only relevant for owned
  /// values or borrow introducers.
  /// For lexical values lifetime is completed at unreachable instructions.
  /// For non-lexical values lifetime is completed at the lifetime boundary.
  /// When \p forceBoundaryCompletion is true, the client is able to guarantee
  /// that lifetime completion of lexical values at the lifetime boundary is
  /// sufficient.
  /// Currently \p forceBoundaryCompletion is used by mem2reg and temprvalueopt
  /// to complete lexical enum values on trivial paths.
  /// Returns true if any new instructions were created to complete the
  /// lifetime.
  ///
  /// TODO: We also need to complete scoped addresses (e.g. store_borrow)!
  LifetimeCompletion
  completeOSSALifetime(SILValue value, bool forceBoundaryCompletion = false) {
    if (value->getOwnershipKind() == OwnershipKind::None)
      return LifetimeCompletion::NoLifetime;

    if (value->getOwnershipKind() != OwnershipKind::Owned) {
      BorrowedValue borrowedValue(value);
      if (!borrowedValue)
        return LifetimeCompletion::NoLifetime;

      if (!borrowedValue.isLocalScope())
        return LifetimeCompletion::AlreadyComplete;
    }
    if (!completedValues.insert(value))
      return LifetimeCompletion::AlreadyComplete;

    return analyzeAndUpdateLifetime(value, forceBoundaryCompletion)
               ? LifetimeCompletion::WasCompleted
               : LifetimeCompletion::AlreadyComplete;
  }

protected:
  bool analyzeAndUpdateLifetime(SILValue value, bool forceBoundaryCompletion);
};

//===----------------------------------------------------------------------===//
// UnreachableLifetimeCompletion
//===----------------------------------------------------------------------===//

/// Fixup OSSA before deleting an unreachable code path.
///
/// Only needed when a code path reaches a no-return function, making the
/// path now partially unreachable. Conditional branch folding requires no fixup
/// because it causes the entire path to become unreachable.
class UnreachableLifetimeCompletion {
  SILFunction *function;

  // If domInfo is nullptr, lifetime completion may attempt to recreate
  // redundant phis, which should be immediately discarded.
  const DominanceInfo *domInfo = nullptr;

  BasicBlockSetVector unreachableBlocks;
  InstructionSet unreachableInsts; // not including those in unreachableBlocks
  ValueSetVector incompleteValues;
  bool updatingLifetimes = false;

public:
  UnreachableLifetimeCompletion(SILFunction *function, DominanceInfo *domInfo)
    : function(function), unreachableBlocks(function),
      unreachableInsts(function), incompleteValues(function) {}

  /// Record information about this unreachable instruction and return true if
  /// ends any simple OSSA lifetimes.
  ///
  /// Note: this must be called in forward order so that lifetime completion
  /// runs from the inside out.
  void visitUnreachableInst(SILInstruction *instruction);

  void visitUnreachableBlock(SILBasicBlock *block) {
    unreachableBlocks.insert(block);
  }

  /// Complete the lifetime of any value defined outside of the unreachable
  /// region that was previously destroyed in the unreachable region.
  bool completeLifetimes();
};

} // namespace swift

#endif
