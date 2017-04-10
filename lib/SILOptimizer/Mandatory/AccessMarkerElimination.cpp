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
/// unconditionally eliminates the markers.
/// 
/// This is an always-on pass for temporary bootstrapping. It allows running
/// test cases through the pipeline and exercising SIL verification before all
/// passes support access markers.
///
/// TODO: DefiniteInitialization needs to be fixed to handle begin/end_access.
/// The immediate goal is to run this pass only at -O. Access markers should not
/// interfere with any -Onone passes.
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
    for (auto &F : *getModule()) {
      // If the function has no access markers, don't invalidate any analyses.
      if (!F.hasAccessMarkers())
        continue;
      
      F.disableAccessMarkers();

      // iterating in reverse eliminates more begin_access users before they
      // need to be replaced.
      for (auto &BB : reversed(F)) {
        for (auto II = BB.rbegin(), IE = BB.rend(); II != IE;) {
          SILInstruction *inst = &*II;
          ++II;

          if (auto beginAccess = dyn_cast<BeginAccessInst>(inst)) {
            beginAccess->replaceAllUsesWith(beginAccess->getSource());
            beginAccess->eraseFromParent();
          }
          if (auto endAccess = dyn_cast<EndAccessInst>(inst)) {
            assert(endAccess->use_empty());
            endAccess->eraseFromParent();
          }
        }
      }

      auto InvalidKind = SILAnalysis::InvalidationKind::Instructions;
      invalidateAnalysis(&F, InvalidKind);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createAccessMarkerElimination() {
  return new AccessMarkerElimination();
}
