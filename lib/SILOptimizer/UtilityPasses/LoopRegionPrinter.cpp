//===--- LoopRegionPrinter.cpp --------------------------------------------===//
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
//
// Simple pass for testing the new loop region dumper analysis. Prints out
// information suitable for checking with filecheck.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-loop-region-printer"

#include "swift/SILOptimizer/Analysis/LoopRegionAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

static llvm::cl::opt<std::string>
    SILViewCFGOnlyFun("sil-loop-region-view-cfg-only-function",
                      llvm::cl::init(""),
                      llvm::cl::desc("Only produce a graphviz file for the "
                                     "loop region info of this function"));

static llvm::cl::opt<std::string>
    SILViewCFGOnlyFuns("sil-loop-region-view-cfg-only-functions",
                       llvm::cl::init(""),
                       llvm::cl::desc("Only produce a graphviz file for the "
                                      "loop region info for the functions "
                                      "whose name contains this substring"));

namespace {

class LoopRegionViewText : public SILModuleTransform {
  void run() override {
    invalidateAll();
    auto *lra = PM->getAnalysis<LoopRegionAnalysis>();

    for (auto &fn : *getModule()) {
      if (fn.isExternalDeclaration())
        continue;
      if (!SILViewCFGOnlyFun.empty() && fn.getName() != SILViewCFGOnlyFun)
        continue;
      if (!SILViewCFGOnlyFuns.empty() &&
          fn.getName().find(SILViewCFGOnlyFuns, 0) == StringRef::npos)
        continue;

      // Ok, we are going to analyze this function. Invalidate all state
      // associated with it so we recompute the loop regions.
      llvm::outs() << "Start @" << fn.getName() << "@\n";
      lra->get(&fn)->dump();
      llvm::outs() << "End @" << fn.getName() << "@\n";
      llvm::outs().flush();
    }
  }
};

class LoopRegionViewCFG : public SILModuleTransform {
  void run() override {
    invalidateAll();
    auto *lra = PM->getAnalysis<LoopRegionAnalysis>();

    for (auto &fn : *getModule()) {
      if (fn.isExternalDeclaration())
        continue;
      if (!SILViewCFGOnlyFun.empty() && fn.getName() != SILViewCFGOnlyFun)
        continue;
      if (!SILViewCFGOnlyFuns.empty() &&
          fn.getName().find(SILViewCFGOnlyFuns, 0) == StringRef::npos)
        continue;

      // Ok, we are going to analyze this function. Invalidate all state
      // associated with it so we recompute the loop regions.
      lra->get(&fn)->viewLoopRegions();
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createLoopRegionViewText() {
  return new LoopRegionViewText();
}

SILTransform *swift::createLoopRegionViewCFG() {
  return new LoopRegionViewCFG();
}
