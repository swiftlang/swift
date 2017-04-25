//===--- AccessMarkerElimination.cpp - Eliminate access markers. ----------===//
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
///
/// This pass eliminates the instructions that demarcate memory access regions.
/// If no memory access markers exist, then the pass does nothing. Otherwise, it
/// unconditionally eliminates all non-dynamic markers (plus any dynamic markers
/// if dynamic exclusivity checking is disabled).
/// 
/// This is an always-on pass for temporary bootstrapping. It allows running
/// test cases through the pipeline and exercising SIL verification before all
/// passes support access markers.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "access-marker-elim"
#include "swift/Basic/Range.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

namespace {

struct AccessMarkerElimination : SILModuleTransform {
  void replaceBeginAccessUsers(BeginAccessInst *beginAccess) {

    // Handle all the uses:
    while (!beginAccess->use_empty()) {
      Operand *op = *beginAccess->use_begin();

      // Delete any associated end_access instructions.
      if (auto endAccess = dyn_cast<EndAccessInst>(op->getUser())) {
        assert(endAccess->use_empty() && "found use of end_access");
        endAccess->eraseFromParent();

        // Forward all other uses to the original address.
      } else {
        op->set(beginAccess->getSource());
      }
    }
  }

  void run() override {
    // Don't bother doing anything unless some kind of exclusivity
    // enforcement is enabled.
    auto &M = *getModule();

    auto preserveAccess = [&M](SILAccessEnforcement enforcement) {
      // Leave dynamic accesses in place, but delete all others.
      return enforcement == SILAccessEnforcement::Dynamic
             && M.getOptions().EnforceExclusivityDynamic;
    };

    bool removedAny = false;

    auto eraseInst = [&removedAny](SILInstruction *inst) {
      removedAny = true;
      return inst->getParent()->erase(inst);
    };

    for (auto &F : M) {
      // Iterating in reverse eliminates more begin_access users before they
      // need to be replaced.
      for (auto &BB : reversed(F)) {
        // Don't cache the begin iterator since we're reverse iterating.
        for (auto II = BB.end(); II != BB.begin();) {
          SILInstruction *inst = &*(--II);

          if (auto beginAccess = dyn_cast<BeginAccessInst>(inst)) {
            // Leave dynamic accesses in place, but delete all others.
            if (preserveAccess(beginAccess->getEnforcement()))
              continue;

            replaceBeginAccessUsers(beginAccess);
            II = eraseInst(beginAccess);
            continue;
          }

          // end_access instructions will be handled when we process the
          // begin_access.

          // begin_unpaired_access instructions will be directly removed and
          // simply replaced with their operand.
          if (auto BUA = dyn_cast<BeginUnpairedAccessInst>(inst)) {
            if (preserveAccess(BUA->getEnforcement()))
              continue;

            BUA->replaceAllUsesWith(BUA->getSource());
            II = eraseInst(BUA);
            continue;
          }
          // end_unpaired_access instructions will be directly removed and
          // simply replaced with their operand.
          if (auto EUA = dyn_cast<EndUnpairedAccessInst>(inst)) {
            if (preserveAccess(EUA->getEnforcement()))
              continue;

            assert(EUA->use_empty() && "use of end_unpaired_access");
            II = eraseInst(EUA);
            continue;
          }
        }
      }

      // Don't invalidate any analyses if we didn't do anything.
      if (!removedAny)
        continue;

      auto InvalidKind = SILAnalysis::InvalidationKind::Instructions;
      invalidateAnalysis(&F, InvalidKind);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createAccessMarkerElimination() {
  return new AccessMarkerElimination();
}
