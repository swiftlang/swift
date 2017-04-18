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
  void run() override {
    // Don't bother doing anything unless some kind of exclusivity
    // enforcement is enabled.
    auto &M = *getModule();
    if (!M.getOptions().isAnyExclusivityEnforcementEnabled())
      return;

    bool removedAny = false;

    for (auto &F : M) {
      // Iterating in reverse eliminates more begin_access users before they
      // need to be replaced.
      for (auto &BB : reversed(F)) {
        // Don't cache the begin iterator since we're reverse iterating.
        for (auto II = BB.end(); II != BB.begin();) {
          SILInstruction *inst = &*(--II);

          if (auto beginAccess = dyn_cast<BeginAccessInst>(inst)) {
            // Leave dynamic accesses in place, but delete all others.
            if (beginAccess->getEnforcement() == SILAccessEnforcement::Dynamic
                && M.getOptions().EnforceExclusivityDynamic)
              continue;

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

            II = BB.erase(beginAccess);
            removedAny = true;
            continue;
          }

          // end_access instructions will be handled when we process the
          // begin_access.
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
