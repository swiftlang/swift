//===--------- UpdateEscapeAnalysis.cpp - Computes escape analysis --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "update-ea"
#include "swift/SILPasses/Passes.h"
#include "swift/SILAnalysis/EscapeAnalysis.h"
#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

namespace {

#ifndef NDEBUG
llvm::cl::opt<bool> PrintEscapes("sil-print-escapes",
                 llvm::cl::desc("Print the result of the escape analysis"),
                 llvm::cl::init(false));
#endif

/// Recomputes the escape information for all functions in the module.
/// For details see EscapeAnalysis.
class UpdateEscapeAnalysis : public SILModuleTransform {

  void run() override {

    DEBUG(llvm::dbgs() << "** UpdateEscapeAnalysis **\n");

    auto *EA = PM->getAnalysis<EscapeAnalysis>();
    EA->recompute();

#ifndef NDEBUG
    if (PrintEscapes) {
      llvm::outs() << "Escape information of module\n";
      for (auto &F : *getModule()) {
        if (!F.isExternalDeclaration()) {
          if (auto *ConnectionGraph = EA->getConnectionGraph(&F)) {
            ConnectionGraph->computeUsePoints();
            ConnectionGraph->print(llvm::outs());
          }
        }
      }
    }
#endif
  }

  StringRef getName() override { return "UpdateEscapeAnalysis"; }
};

} // end anonymous namespace

SILTransform *swift::createUpdateEscapeAnalysis() {
  return new UpdateEscapeAnalysis();
}
