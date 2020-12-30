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
/// balance the retain count, it is only sound for ownership-SSA. The pass
/// itself is mostly for testing the underlying functionality which can also be
/// invoked as a utility for any owned value.
///
/// TODO: Cleanup the resulting SIL by deleting instructions that produce dead
/// values (after removing its copies).
/// ===----------------------------------------------------------------------===

#define DEBUG_TYPE "copy-propagation"

#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CanonicalOSSALifetime.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// CopyPropagation: Top-Level Function Transform.
//===----------------------------------------------------------------------===//

namespace {
class CopyPropagation : public SILFunctionTransform {
  /// The entry point to this function transformation.
  void run() override;
};
} // end anonymous namespace

/// Top-level pass driver.
void CopyPropagation::run() {
  auto *f = getFunction();
  
  // Debug label for unit testing.
  LLVM_DEBUG(llvm::dbgs() << "*** CopyPropagation: " << f->getName() << "\n");

  // This algorithm fundamentally assumes ownership.
  if (!f->hasOwnership())
    return;

  // Driver: Find all copied defs.
  llvm::SmallSetVector<SILValue, 16> copiedDefs;
  for (auto &bb : *f) {
    for (auto &i : bb) {
      if (auto *copy = dyn_cast<CopyValueInst>(&i))
        copiedDefs.insert(getCanonicalCopiedDef(copy));
    }
  }
  // Perform copy propgation for each copied value.
  CanonicalOSSALifetime lifetime;
  for (auto &def : copiedDefs) {
    canonicalizeValueLifetime(def, lifetime);
  }
  if (lifetime.changed) {
    invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    DeadEndBlocks deBlocks(f);
    f->verifyOwnership(&deBlocks);
  }
}

SILTransform *swift::createCopyPropagation() { return new CopyPropagation(); }
