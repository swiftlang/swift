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
  virtual bool isFullElimination() = 0;

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

  bool shouldPreserveAccess(SILAccessEnforcement enforcement) {
    auto &M = *getModule();
    switch (enforcement) {
    case SILAccessEnforcement::Unknown:
      return false;
    case SILAccessEnforcement::Static:
      // Even though static enforcement is already performed, this flag is
      // useful to control marker preservation for now.
      return M.getOptions().EnforceExclusivityStatic;
    case SILAccessEnforcement::Dynamic:
      return M.getOptions().EnforceExclusivityDynamic;
    case SILAccessEnforcement::Unsafe:
      return false;
    }
  };

  void run() override {
    auto &M = *getModule();

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
            if (shouldPreserveAccess(beginAccess->getEnforcement()))
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
            if (shouldPreserveAccess(BUA->getEnforcement()))
              continue;

            BUA->replaceAllUsesWith(BUA->getSource());
            II = eraseInst(BUA);
            continue;
          }
          // end_unpaired_access instructions will be directly removed and
          // simply replaced with their operand.
          if (auto EUA = dyn_cast<EndUnpairedAccessInst>(inst)) {
            if (shouldPreserveAccess(EUA->getEnforcement()))
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

struct InactiveAccessMarkerElimination : AccessMarkerElimination {
  virtual bool isFullElimination() { return false; }
};

struct FullAccessMarkerElimination : AccessMarkerElimination {
  virtual bool isFullElimination() { return true; }
};

} // end anonymous namespace

SILTransform *swift::createInactiveAccessMarkerElimination() {
  return new InactiveAccessMarkerElimination();
}

SILTransform *swift::createFullAccessMarkerElimination() {
  return new FullAccessMarkerElimination();
}
