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

static llvm::cl::opt<std::string> &SILViewCFGOnlyFun() {
  auto &opts = llvm::cl::getRegisteredOptions();
  auto it = opts.find("sil-loop-region-view-cfg-only-function");
  if (it != opts.end()) {
    return *static_cast<llvm::cl::opt<std::string>*>(it->second);
  }
  static auto *opt = new llvm::cl::opt<std::string>(
      "sil-loop-region-view-cfg-only-function",
      llvm::cl::init(""),
      llvm::cl::desc("Only produce a graphviz file for the "
                     "loop region info of this function"));
  return *opt;
}
static auto &EarlyInitSILViewCFGOnlyFun = SILViewCFGOnlyFun();

static llvm::cl::opt<std::string> &SILViewCFGOnlyFuns() {
  auto &opts = llvm::cl::getRegisteredOptions();
  auto it = opts.find("sil-loop-region-view-cfg-only-functions");
  if (it != opts.end()) {
    return *static_cast<llvm::cl::opt<std::string>*>(it->second);
  }
  static auto *opt = new llvm::cl::opt<std::string>(
      "sil-loop-region-view-cfg-only-functions",
      llvm::cl::init(""),
      llvm::cl::desc("Only produce a graphviz file for the "
                     "loop region info for the functions "
                     "whose name contains this substring"));
  return *opt;
}
static auto &EarlyInitSILViewCFGOnlyFuns = SILViewCFGOnlyFuns();

namespace {

class LoopRegionViewText : public SILModuleTransform {
  void run() override {
    invalidateAll();
    auto *lra = PM->getAnalysis<LoopRegionAnalysis>();

    for (auto &fn : *getModule()) {
      if (fn.isExternalDeclaration())
        continue;
      if (!SILViewCFGOnlyFun().empty() && fn.getName() != SILViewCFGOnlyFun())
        continue;
      if (!SILViewCFGOnlyFuns().empty() &&
          !fn.getName().contains(SILViewCFGOnlyFuns()))
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
      if (!SILViewCFGOnlyFun().empty() && fn.getName() != SILViewCFGOnlyFun())
        continue;
      if (!SILViewCFGOnlyFuns().empty() &&
          !fn.getName().contains(SILViewCFGOnlyFuns()))
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
