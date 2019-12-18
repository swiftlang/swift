//===--- EscapeAnalysisDumper.cpp - Dumps the escape analysis -------------===//
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

#define DEBUG_TYPE "dump-ea"
#include "swift/SILOptimizer/Analysis/EscapeAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

// For manual debugging, dump graphs in DOT format.
llvm::cl::opt<bool> EnableGraphWriter(
    "escapes-enable-graphwriter", llvm::cl::init(false),
    llvm::cl::desc("With -escapes-dump, also write .dot files."));

namespace {

/// Dumps the escape information of all functions in the module.
/// Only dumps if the compiler is built with assertions.
/// For details see EscapeAnalysis.
class EscapeAnalysisDumper : public SILModuleTransform {

  void run() override {
    LLVM_DEBUG(llvm::dbgs() << "** EscapeAnalysisDumper **\n");

#ifndef NDEBUG
    auto *EA = PM->getAnalysis<EscapeAnalysis>();

    llvm::outs() << "Escape information of module\n";
    for (auto &F : *getModule()) {
      if (!F.isExternalDeclaration()) {
        auto *ConnectionGraph = EA->getConnectionGraph(&F);
        ConnectionGraph->print(llvm::outs());
        if (EnableGraphWriter)
          ConnectionGraph->dumpCG();
      }
    }
#endif
  }

};

} // end anonymous namespace

SILTransform *swift::createEscapeAnalysisDumper() {
  return new EscapeAnalysisDumper();
}
