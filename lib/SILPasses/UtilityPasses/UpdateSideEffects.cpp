//===--- SideEffectOpt.cpp - Test the side effect analysis ----------------===//
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
#include "swift/SILAnalysis/EscapeAnalysis.h"
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

namespace {

#ifndef NDEBUG
llvm::cl::opt<bool> PrintSideEffects("sil-print-side-effects",
                 llvm::cl::desc("Print the result of the side-effect analysis"),
                 llvm::cl::init(false));

llvm::cl::opt<bool> PrintEscapes("sil-print-escapes",
                 llvm::cl::desc("Print the result of the escape analysis"),
                 llvm::cl::init(false));
#endif


/// Recomputes the side-effect and escape information for all functions in the
/// module. For details see SideEffectAnalysis and EscapeAnalysis.
/// TODO: Consider having separate passes for SideEffectAnalysis and
/// EscapeAnalysis.
class UpdateSideEffects : public SILModuleTransform {

  void run() override {

    DEBUG(llvm::dbgs() << "** UpdateSideEffects **\n");

    auto *SEA = PM->getAnalysis<SideEffectAnalysis>();
    SEA->recompute();

    auto *EA = PM->getAnalysis<EscapeAnalysis>();
    EA->recompute();

#ifndef NDEBUG
    if (PrintSideEffects) {
      llvm::outs() << "Side effects of module\n";
      for (auto &F : *getModule()) {
        llvm::outs() << "  sil @" << F.getName() << '\n';
        const auto &Effects = SEA->getEffects(&F);
        llvm::outs() << "    <" << Effects << ">\n";
      }
    }
    if (PrintEscapes) {
      llvm::outs() << "Escape information of module\n";
      for (auto &F : *getModule()) {
        if (!F.isExternalDeclaration()) {
          auto *ConnectionGraph = EA->getConnectionGraph(&F);
          ConnectionGraph->print(llvm::outs());
        }
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
