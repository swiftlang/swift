//===--- CanonicalOSSALifetime.h - Canonicalize OSSA lifetimes --*- C++ -*-===//
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
///
/// Canonicalize the copies and destroys of a single owned or guaranteed OSSA
/// value.
///
/// This top-level API rewrites the extended lifetime of a SILValue:
///
///     void canonicalizeValueLifetime(SILValue def, CanonicalOSSALifetime&)
///
/// The extended lifetime transitively includes the uses of `def` itself along
/// with the uses of any copies of `def`.
///
/// FIXME: Canonicalization currently bails out if any uses of the def has
/// OperandOwnership::PointerEscape. Once project_box is protected by a borrow
/// scope and mark_dependence is associated with an end_dependence,
/// canonicalization will work everywhere as intended.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_CANONICALOSSALIFETIME_H
#define SWIFT_SILOPTIMIZER_UTILS_CANONICALOSSALIFETIME_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Utils/PrunedLiveness.h"

namespace swift {

struct CanonicalOSSALifetime;

/// Top-Level API: rewrites copies and destroys within \p def's extended
/// lifetime. \p lifetime caches transient analysis state across multiple calls
/// and indicates whether any SILAnalyses must be invalidated.
///
/// Return false if the OSSA structure cannot be recognized (with a proper OSSA
/// representation this will always return true).
bool canonicalizeValueLifetime(SILValue def, CanonicalOSSALifetime &lifetime);

/// Find the original definition of a potentially copied value.
///
/// This use-def walk must be consistent with the def-use walks performed within
/// the CanonicalOSSALifetime implementation.
inline SILValue getCanonicalCopiedDef(SILValue v) {
  while (true) {
    if (auto *copy = dyn_cast<CopyValueInst>(v)) {
      v = copy->getOperand();
      continue;
    }
    return v;
  }
}

/// Information about consumes on the extended-lifetime boundary. Consuming uses
/// within the lifetime are not included--they will consume a copy after
/// rewriting.
///
/// This result remains valid during copy rewriting. The only instructions
/// referenced it contains are consumes that cannot be deleted.
class CanonicalOSSAConsumeInfo {
  // Map blocks on the lifetime boundary to the last consuming instruction.
  llvm::SmallDenseMap<SILBasicBlock *, SILInstruction *, 4> finalBlockConsumes;

public:
  bool empty() const { return finalBlockConsumes.empty(); }

  void clear() { finalBlockConsumes.clear(); }

  void recordFinalConsume(SILInstruction *inst) {
    assert(!finalBlockConsumes.count(inst->getParent()));
    finalBlockConsumes[inst->getParent()] = inst;
  }

  // Return true if this instruction is marked as a final consume point of the
  // current def's live range. A consuming instruction can only be claimed once
  // because instructions like `tuple` can consume the same value via multiple
  // operands.
  bool claimConsume(SILInstruction *inst) {
    auto destroyPos = finalBlockConsumes.find(inst->getParent());
    if (destroyPos != finalBlockConsumes.end() && destroyPos->second == inst) {
      finalBlockConsumes.erase(destroyPos);
      return true;
    }
    return false;
  }

  SWIFT_ASSERT_ONLY_DECL(void dump() const LLVM_ATTRIBUTE_USED);
};

/// Canonicalize OSSA lifetimes.
///
/// Allows the allocation of analysis state to be reused across calls to
/// canonicalizeValueLifetime().
struct CanonicalOSSALifetime {
  // Current copied def for which this state describes the liveness.
  SILValue currDef;

  // Cumulatively, have any instructions been modified by canonicalization?
  bool changed = false;

  // Pruned liveness for the extended live range including copies. For this
  // purpose, only consuming instructions are considered "lifetime
  // ending". end_borrows do not end a liverange that may include owned copies.
  PrunedLiveness liveness;

  // Original points in the CFG where the current value's lifetime ends. This
  // includes any point in which the value is consumed or destroyed. For
  // guaranteed values, it also includes points where the borrow scope
  // ends. A backward walk from these blocks must discover all uses on paths
  // that lead to a return or throw.
  //
  // These blocks are not necessarily in the pruned live blocks since
  // pruned liveness does not consider destroy_values.
  llvm::SmallSetVector<SILBasicBlock *, 8> lifetimeEndBlocks;

public:
  CanonicalOSSAConsumeInfo consumes;

  SILValue def() const { return currDef; }

  void initDef(SILValue def) {
    assert(lifetimeEndBlocks.empty() && liveness.empty());
    consumes.clear();

    currDef = def;
    liveness.initializeDefBlock(def->getParentBlock());
  }

  void clearLiveness() {
    lifetimeEndBlocks.clear();
    liveness.clear();
  }

  bool isChanged() const { return changed; }

  void setChanged() { changed = true; }

  void updateLivenessForUse(Operand *use) {
    // Because this liverange may include owned copies, only record consuming
    // instructions as "lifetime ending".
    bool consuming =
      use->isLifetimeEnding()
      && (use->get().getOwnershipKind() == OwnershipKind::Owned);
    liveness.updateForUse(use, consuming);
  }

  PrunedLiveBlocks::IsLive getBlockLiveness(SILBasicBlock *bb) const {
    return liveness.getBlockLiveness(bb);
  }

  enum IsInterestingUser { NonUser, NonConsumingUse, ConsumingUse };
  IsInterestingUser isInterestingUser(SILInstruction *user) const {
    // Translate PrunedLiveness "lifetime-ending" uses to consuming uses. For
    // the purpose of an extended liverange that includes owned copies, an
    // end_borrow is not "lifetime-ending".
    switch (liveness.isInterestingUser(user)) {
    case PrunedLiveness::NonUser:
      return NonUser;
    case PrunedLiveness::NonLifetimeEndingUse:
      return NonConsumingUse;
    case PrunedLiveness::LifetimeEndingUse:
      return ConsumingUse;
    }
  }

  void recordLifetimeEnd(Operand *use) {
    lifetimeEndBlocks.insert(use->getUser()->getParent());
  }

  ArrayRef<SILBasicBlock *> getLifetimeEndBlocks() const {
    return lifetimeEndBlocks.getArrayRef();
  }
};

} // end namespace swift

#endif
