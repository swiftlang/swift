//===--------- CopyPropagation.cpp - Remove redundant SSA copies. ---------===//
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
/// SSA Copy propagation pass to remove unnecessary copy_value and destroy_value
/// instructions.
///
/// Because this algorithm rewrites copies and destroys without attempting to
/// balance the retain count, it is only sound when SIL is in ownership-SSA
/// form. The pass itself is mostly for testing the underlying functionality
/// which can also be invoked as a utility for any owned value.
///
/// TODO: Cleanup the resulting SIL by deleting instructions that produce dead
/// values (after removing its copies).
///
/// ===----------------------------------------------------------------------===

#define DEBUG_TYPE "copy-propagation"

#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SILOptimizer/Analysis/DeadEndBlocksAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CanonicalOSSALifetime.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"

using namespace swift;

// This only applies to -O copy-propagation.
llvm::cl::opt<bool>
    EnableRewriteBorrows("canonical-ossa-rewrite-borrows",
                         llvm::cl::init(false),
                         llvm::cl::desc("Enable rewriting borrow scopes"));

//===----------------------------------------------------------------------===//
// CopyPropagation: Top-Level Function Transform.
//===----------------------------------------------------------------------===//

namespace {
class CopyPropagation : public SILFunctionTransform {
  /// True if debug_value instructions should be pruned.
  bool pruneDebug;
  /// True of all values should be canonicalized.
  bool canonicalizeAll;
  /// If true, then borrow scopes will be canonicalized, allowing copies of
  /// guaranteed values to be optimized. Does *not* shrink the borrow scope.
  bool canonicalizeBorrows;
  /// If true, then new destroy_value instructions will be poison.
  bool poisonRefs;

public:
  CopyPropagation(bool pruneDebug, bool canonicalizeAll,
                  bool canonicalizeBorrows, bool poisonRefs)
      : pruneDebug(pruneDebug), canonicalizeAll(canonicalizeAll),
        canonicalizeBorrows(canonicalizeBorrows), poisonRefs(poisonRefs) {}

  /// The entry point to this function transformation.
  void run() override;
};
} // end anonymous namespace

static bool isCopyDead(CopyValueInst *copy, bool pruneDebug) {
  for (Operand *use : copy->getUses()) {
    auto *user = use->getUser();
    if (isa<DestroyValueInst>(user)) {
      continue;
    }
    if (pruneDebug && isa<DebugValueInst>(user)) {
      continue;
    }
    return false;
  }
  return true;
}

static bool isBorrowDead(BeginBorrowInst *borrow) {
  return llvm::all_of(borrow->getUses(), [](Operand *use) {
      SILInstruction *user = use->getUser();
      return isa<EndBorrowInst>(user) || user->isDebugInstruction();
    });
}

/// Top-level pass driver.
void CopyPropagation::run() {
  auto *f = getFunction();
  auto *accessBlockAnalysis = getAnalysis<NonLocalAccessBlockAnalysis>();
  auto *dominanceAnalysis = getAnalysis<DominanceAnalysis>();

  // Debug label for unit testing.
  LLVM_DEBUG(llvm::dbgs() << "*** CopyPropagation: " << f->getName() << "\n");

  // This algorithm fundamentally assumes ownership.
  if (!f->hasOwnership())
    return;

  // Driver: Find all copied defs.
  llvm::SmallSetVector<SILValue, 16> copiedDefs;
  for (auto &bb : *f) {
    for (auto &i : bb) {
      if (auto *copy = dyn_cast<CopyValueInst>(&i)) {
        copiedDefs.insert(
            CanonicalizeOSSALifetime::getCanonicalCopiedDef(copy));
      } else if (canonicalizeAll) {
        if (auto *destroy = dyn_cast<DestroyValueInst>(&i)) {
          copiedDefs.insert(CanonicalizeOSSALifetime::getCanonicalCopiedDef(
              destroy->getOperand()));
        }
      }
    }
  }
  // Push copy_value instructions above their struct_extract operands by
  // inserting destructures.
  //
  // copiedDefs be be modified, but it never shrinks
  for (unsigned idx = 0; idx < copiedDefs.size(); ++idx) {
    SILValue def = copiedDefs[idx];
    auto *copy = dyn_cast<CopyValueInst>(def);
    if (!copy)
      continue;

    auto *extract = dyn_cast<StructExtractInst>(copy->getOperand());
    if (!extract
        || SILValue(extract).getOwnershipKind() != OwnershipKind::Guaranteed)
      continue;

    // Bail-out if we won't rewrite borrows because that currently regresses
    // Breadcrumbs.MutatedUTF16ToIdx.Mixed/Breadcrumbs.MutatedIdxToUTF16.Mixed.
    // Also, mandatory copyprop does not need to rewrite destructures.
    if (!canonicalizeBorrows)
      continue;

    if (SILValue destructuredResult = convertExtractToDestructure(extract)) {
      // Remove to-be-deleted instructions from copiedDeds. The extract cannot
      // be in the copiedDefs set since getCanonicalCopiedDef does not allow a
      // guaranteed projection to be a canonical def.
      copiedDefs.remove(copy);
      --idx; // point back to the current element, which was erased.

      // TODO: unfortunately SetVector has no element replacement.
      copiedDefs.insert(destructuredResult);

      auto *destructure = cast<DestructureStructInst>(
          destructuredResult.getDefiningInstruction());
      auto *newCopy = cast<CopyValueInst>(destructure->getOperand());
      copiedDefs.insert(
          CanonicalizeOSSALifetime::getCanonicalCopiedDef(newCopy));

      LLVM_DEBUG(llvm::dbgs() << "Destructure Conversion:\n"
                              << *extract << "  to " << *destructure);
      // Delete both the copy and the extract.
      InstructionDeleter().recursivelyDeleteUsersIfDead(extract);
    }
  }
  // Perform copy propgation for each copied value.
  CanonicalizeOSSALifetime canonicalizer(pruneDebug, canonicalizeBorrows,
                                         poisonRefs, accessBlockAnalysis,
                                         dominanceAnalysis);
  // Cleanup dead copies. If getCanonicalCopiedDef returns a copy (because the
  // copy's source operand is unrecgonized), then the copy is itself treated
  // like a def and may be dead after canonicalization.
  llvm::SmallVector<SILInstruction *, 4> deadCopies;
  for (auto &def : copiedDefs) {
    // Canonicalized this def.
    canonicalizer.canonicalizeValueLifetime(def);

    if (auto *copy = dyn_cast<CopyValueInst>(def)) {
      if (isCopyDead(copy, pruneDebug)) {
        deadCopies.push_back(copy);
      }
    }
    // Dead borrow scopes must be removed as uses before canonicalizing the
    // outer copy.
    if (auto *borrow = dyn_cast<BeginBorrowInst>(def)) {
      if (isBorrowDead(borrow)) {
        borrow->setOperand(SILUndef::get(borrow->getType(), *f));
        deadCopies.push_back(borrow);
      }
    }
    // Canonicalize any new outer copy.
    if (SILValue outerCopy = canonicalizer.createdOuterCopy()) {
      SILValue outerDef = canonicalizer.getCanonicalCopiedDef(outerCopy);
      canonicalizer.canonicalizeValueLifetime(outerDef);
    }
    // TODO: also canonicalize any lifetime.persistentCopies like separate owned
    // live ranges.
  }
  if (canonicalizer.hasChanged() || !deadCopies.empty()) {
    InstructionDeleter deleter;
    for (auto *copyOrBorrow : deadCopies) {
      deleter.recursivelyDeleteUsersIfDead(copyOrBorrow);
    }
    // Preserves NonLocalAccessBlockAnalysis.
    accessBlockAnalysis->lockInvalidation();
    invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    accessBlockAnalysis->unlockInvalidation();
    if (f->getModule().getOptions().VerifySILOwnership) {
      auto *deBlocksAnalysis = getAnalysis<DeadEndBlocksAnalysis>();
      f->verifyOwnership(deBlocksAnalysis->get(f));
    }
  }
}

SILTransform *swift::createMandatoryCopyPropagation() {
  return new CopyPropagation(/*pruneDebug*/ true, /*canonicalizeAll*/ true,
                             /*canonicalizeBorrows*/ false,
                             /*poisonRefs*/ true);
}

SILTransform *swift::createCopyPropagation() {
  return new CopyPropagation(/*pruneDebug*/ true, /*canonicalizeAll*/ false,
                             /*canonicalizeBorrows*/ EnableRewriteBorrows,
                             /*poisonRefs*/ false);
}

