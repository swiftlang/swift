//===------ UpdateSideEffects.cpp - Computes the side effect analysis -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "update-sea"
#include "swift/SILPasses/Passes.h"
#include "swift/SILAnalysis/SideEffectAnalysis.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

namespace {

#ifndef NDEBUG
llvm::cl::opt<bool> PrintSideEffects("sil-print-side-effects",
                 llvm::cl::desc("Print the result of the side-effect analysis"),
                 llvm::cl::init(false));
#endif

/// Recomputes the side-effect information for all functions in the module.
/// For details see SideEffectAnalysis.
class UpdateSideEffects : public SILModuleTransform {

  void run() override {

    DEBUG(llvm::dbgs() << "** UpdateSideEffects **\n");

    auto *SEA = PM->getAnalysis<SideEffectAnalysis>();
    SEA->recompute();

#ifndef NDEBUG
    if (PrintSideEffects) {
      llvm::outs() << "Side effects of module\n";
      for (auto &F : *getModule()) {
        llvm::outs() << "  sil @" << F.getName() << '\n';
        const auto &Effects = SEA->getEffects(&F);
        llvm::outs() << "    <" << Effects << ">\n";
      }
    }
#endif
  }

  StringRef getName() override { return "UpdateSideEffects"; }
};

} // end anonymous namespace

SILTransform *swift::createUpdateSideEffects() {
  return new UpdateSideEffects();
}
