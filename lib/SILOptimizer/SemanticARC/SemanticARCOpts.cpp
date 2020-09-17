//===--- SemanticARCOpts.cpp ----------------------------------------------===//
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

#define DEBUG_TYPE "sil-semantic-arc-opts"

#include "SemanticARCOptVisitor.h"
#include "Transforms.h"

#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;
using namespace swift::semanticarc;

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

// Even though this is a mandatory pass, it is rerun after deserialization in
// case DiagnosticConstantPropagation exposed anything new in this assert
// configuration.
struct SemanticARCOpts : SILFunctionTransform {
  bool guaranteedOptsOnly;

  SemanticARCOpts(bool guaranteedOptsOnly)
      : guaranteedOptsOnly(guaranteedOptsOnly) {}

  void run() override {
    SILFunction &f = *getFunction();

    // Return early if we are not performing OSSA optimizations.
    if (!f.getModule().getOptions().EnableOSSAOptimizations)
      return;

    // Make sure we are running with ownership verification enabled.
    assert(f.getModule().getOptions().VerifySILOwnership &&
           "Can not perform semantic arc optimization unless ownership "
           "verification is enabled");

    SemanticARCOptVisitor visitor(f, guaranteedOptsOnly);

    // Add all the results of all instructions that we want to visit to the
    // worklist.
    for (auto &block : f) {
      for (auto &inst : block) {
        if (SemanticARCOptVisitor::shouldVisitInst(&inst)) {
          for (SILValue v : inst.getResults()) {
            visitor.worklist.insert(v);
          }
        }
      }
    }

    // Then process the worklist, performing peepholes.
    bool eliminatedARCInst = visitor.optimize();

    // Now that we have seeded the map of phis to incoming values that could be
    // converted to guaranteed, ignoring the phi, try convert those phis to be
    // guaranteed.
    if (tryConvertOwnedPhisToGuaranteedPhis(visitor.ctx)) {
      invalidateAnalysis(
          SILAnalysis::InvalidationKind::BranchesAndInstructions);
      return;
    }

    // Otherwise, we only deleted instructions and did not touch phis.
    if (eliminatedARCInst)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
};

} // end anonymous namespace

SILTransform *swift::createSemanticARCOpts() {
  return new SemanticARCOpts(false /*guaranteed*/);
}

SILTransform *swift::createGuaranteedARCOpts() {
  return new SemanticARCOpts(true /*guaranteed*/);
}
