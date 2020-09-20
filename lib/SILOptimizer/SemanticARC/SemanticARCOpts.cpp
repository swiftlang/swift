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

#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace swift::semanticarc;

namespace {

/// An enum used so that at the command line, we can override
enum class TransformToPerformKind {
  Peepholes,
  OwnedToGuaranteedPhi,
};

} // anonymous namespace

static llvm::cl::list<TransformToPerformKind> TransformsToPerform(
    llvm::cl::values(
        clEnumValN(TransformToPerformKind::Peepholes,
                   "sil-semantic-arc-peepholes",
                   "Perform ARC canonicalizations and peepholes"),
        clEnumValN(TransformToPerformKind::OwnedToGuaranteedPhi,
                   "sil-semantic-arc-owned-to-guaranteed-phi",
                   "Perform Owned To Guaranteed Phi. NOTE: Seeded by peephole "
                   "optimizer for compile time saving purposes, so run this "
                   "after running peepholes)")),
    llvm::cl::desc(
        "For testing purposes only run the specified list of semantic arc "
        "optimization. If the list is empty, we run all transforms"));

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

#ifndef NDEBUG
  void performCommandlineSpecifiedTransforms(SemanticARCOptVisitor &visitor) {
    for (auto transform : TransformsToPerform) {
      switch (transform) {
      case TransformToPerformKind::Peepholes:
        if (performPeepholes(visitor)) {
          invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
        }
        continue;
      case TransformToPerformKind::OwnedToGuaranteedPhi:
        if (tryConvertOwnedPhisToGuaranteedPhis(visitor.ctx)) {
          invalidateAnalysis(
              SILAnalysis::InvalidationKind::BranchesAndInstructions);
        }
        continue;
      }
    }
  }
#endif

  bool performPeepholes(SemanticARCOptVisitor &visitor) {
    // Add all the results of all instructions that we want to visit to the
    // worklist.
    for (auto &block : *getFunction()) {
      for (auto &inst : block) {
        if (SemanticARCOptVisitor::shouldVisitInst(&inst)) {
          for (SILValue v : inst.getResults()) {
            visitor.worklist.insert(v);
          }
        }
      }
    }
    // Then process the worklist, performing peepholes.
    return visitor.optimize();
  }

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

#ifndef NDEBUG
    // If we are being asked for testing purposes to run a series of transforms
    // expressed on the command line, run that and return.
    if (!TransformsToPerform.empty()) {
      return performCommandlineSpecifiedTransforms(visitor);
    }
#endif

    // Otherwise, perform our standard optimizations.
    bool didEliminateARCInsts = performPeepholes(visitor);

    // Now that we have seeded the map of phis to incoming values that could be
    // converted to guaranteed, ignoring the phi, try convert those phis to be
    // guaranteed.
    if (tryConvertOwnedPhisToGuaranteedPhis(visitor.ctx)) {
      // We return here early to save a little compile time so we do not
      // invalidate analyses redundantly.
      return invalidateAnalysis(
          SILAnalysis::InvalidationKind::BranchesAndInstructions);
    }

    // Otherwise, we only deleted instructions and did not touch phis.
    if (didEliminateARCInsts)
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
