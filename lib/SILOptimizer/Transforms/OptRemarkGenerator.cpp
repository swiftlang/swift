//===--- OptRemarkGenerator.cpp -------------------------------------------===//
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

#define DEBUG_TYPE "sil-opt-remark-gen"

#include "swift/SIL/OptimizationRemark.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                        Opt Remark Generator Visitor
//===----------------------------------------------------------------------===//

namespace {

struct OptRemarkGeneratorInstructionVisitor
    : public SILInstructionVisitor<OptRemarkGeneratorInstructionVisitor> {
  SILModule &mod;
  RCIdentityFunctionInfo &rcfi;
  OptRemark::Emitter ORE;

  OptRemarkGeneratorInstructionVisitor(SILModule &mod,
                                       RCIdentityFunctionInfo &rcfi)
      : mod(mod), rcfi(rcfi), ORE(DEBUG_TYPE, mod) {}

  void visitStrongRetainInst(StrongRetainInst *sri);
  void visitStrongReleaseInst(StrongReleaseInst *sri);
  void visitRetainValueInst(RetainValueInst *rvi);
  void visitReleaseValueInst(ReleaseValueInst *rvi);
  void visitSILInstruction(SILInstruction *) {}
};

} // anonymous namespace

void OptRemarkGeneratorInstructionVisitor::visitStrongRetainInst(
    StrongRetainInst *sri) {
  ORE.emit([&]() {
    using namespace OptRemark;
    SILValue root = rcfi.getRCIdentityRoot(sri->getOperand());
    SmallVector<Argument, 8> inferredArgs;
    bool foundArgs = Argument::inferArgumentsForValue(
        ArgumentKeyKind::Note, "on value:", root, [&](Argument arg) {
          inferredArgs.push_back(arg);
          return true;
        });
    (void)foundArgs;

    // Retains begin a lifetime scope so we infer scan forward.
    auto remark = RemarkMissed("memory-management", *sri,
                               SourceLocInferenceBehavior::ForwardScanOnly)
                  << "Found retain:";
    if (inferredArgs.empty()) {
      remark << Argument({ArgumentKeyKind::ParentLocNote, "InferValueFailure"},
                         "Unable to infer any values being retained.");
    } else {
      for (auto arg : inferredArgs) {
        remark << arg;
      }
    }
    return remark;
  });
}

void OptRemarkGeneratorInstructionVisitor::visitStrongReleaseInst(
    StrongReleaseInst *sri) {
  ORE.emit([&]() {
    using namespace OptRemark;
    // Releases end a lifetime scope so we infer scan backward.
    SILValue root = rcfi.getRCIdentityRoot(sri->getOperand());
    SmallVector<Argument, 8> inferredArgs;
    bool foundArgs = Argument::inferArgumentsForValue(
        ArgumentKeyKind::Note, "on value:", root, [&](Argument arg) {
          inferredArgs.push_back(arg);
          return true;
        });
    (void)foundArgs;
    auto remark = RemarkMissed("memory-management", *sri,
                               SourceLocInferenceBehavior::BackwardScanOnly)
                  << "Found release:";
    if (inferredArgs.empty()) {
      remark << Argument({ArgumentKeyKind::ParentLocNote, "InferValueFailure"},
                         "Unable to infer any values being released.");
    } else {
      for (auto arg : inferredArgs) {
        remark << arg;
      }
    }
    return remark;
  });
}

void OptRemarkGeneratorInstructionVisitor::visitRetainValueInst(
    RetainValueInst *rvi) {
  ORE.emit([&]() {
    using namespace OptRemark;
    SILValue root = rcfi.getRCIdentityRoot(rvi->getOperand());
    SmallVector<Argument, 8> inferredArgs;
    bool foundArgs = Argument::inferArgumentsForValue(
        ArgumentKeyKind::Note, "on value:", root, [&](Argument arg) {
          inferredArgs.push_back(arg);
          return true;
        });
    (void)foundArgs;

    // Retains begin a lifetime scope, so we infer scan forwards.
    auto remark = RemarkMissed("memory-management", *rvi,
                               SourceLocInferenceBehavior::ForwardScanOnly)
                  << "Found retain:";
    if (inferredArgs.empty()) {
      remark << Argument({ArgumentKeyKind::ParentLocNote, "InferValueFailure"},
                         "Unable to infer any values being retained.");
    } else {
      for (auto arg : inferredArgs) {
        remark << arg;
      }
    }
    return remark;
  });
}

void OptRemarkGeneratorInstructionVisitor::visitReleaseValueInst(
    ReleaseValueInst *rvi) {
  ORE.emit([&]() {
    using namespace OptRemark;
    SILValue root = rcfi.getRCIdentityRoot(rvi->getOperand());
    SmallVector<Argument, 8> inferredArgs;
    bool foundArgs = Argument::inferArgumentsForValue(
        ArgumentKeyKind::Note, "on value:", root, [&](Argument arg) {
          inferredArgs.push_back(arg);
          return true;
        });
    (void)foundArgs;

    // Releases end a lifetime scope so we infer scan backward.
    auto remark = RemarkMissed("memory-management", *rvi,
                               SourceLocInferenceBehavior::BackwardScanOnly)
                  << "Found release:";
    if (inferredArgs.empty()) {
      remark << Argument({ArgumentKeyKind::ParentLocNote, "InferValueFailure"},
                         "Unable to infer any values being released.");
    } else {
      for (auto arg : inferredArgs) {
        remark << arg;
      }
    }

    return remark;
  });
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class OptRemarkGenerator : public SILFunctionTransform {
  ~OptRemarkGenerator() override {}

  bool isOptRemarksEnabled() {
    // TODO: Put this on LangOpts as a helper.
    auto &langOpts = getFunction()->getASTContext().LangOpts;

    // If we have a remark streamer, emit everything.
    return bool(langOpts.OptimizationRemarkMissedPattern) ||
           bool(langOpts.OptimizationRemarkPassedPattern) ||
           getFunction()->getModule().getSILRemarkStreamer();
  }

  /// The entry point to the transformation.
  void run() override {
    if (!isOptRemarksEnabled())
      return;

    auto *fn = getFunction();
    LLVM_DEBUG(llvm::dbgs() << "Visiting: " << fn->getName() << "\n");
    auto &rcfi = *getAnalysis<RCIdentityAnalysis>()->get(fn);
    OptRemarkGeneratorInstructionVisitor visitor(fn->getModule(), rcfi);
    for (auto &block : *fn) {
      for (auto &inst : block) {
        visitor.visit(&inst);
      }
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createOptRemarkGenerator() {
  return new OptRemarkGenerator();
}
