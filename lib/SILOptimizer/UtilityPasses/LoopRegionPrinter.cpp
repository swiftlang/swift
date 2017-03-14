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

using namespace swift;

namespace {

class LoopRegionViewText : public SILModuleTransform {
  void run() override {
    invalidateAll();
    LoopRegionAnalysis *LRA = PM->getAnalysis<LoopRegionAnalysis>();
    for (auto &Fn : *getModule()) {
      if (Fn.isExternalDeclaration()) continue;

      llvm::outs() << "@" << Fn.getName() << "@\n";
      LRA->get(&Fn)->dump();
      llvm::outs() << "\n";
      llvm::outs().flush();
    }
  }

  StringRef getName() override { return "LoopRegionViewText"; }
};

class LoopRegionViewCFG : public SILModuleTransform {
  void run() override {

    LoopRegionAnalysis *LRA = PM->getAnalysis<LoopRegionAnalysis>();

    auto *M = getModule();
    for (auto &Fn : M->getFunctions()) {
      if (Fn.isExternalDeclaration())
        continue;
      LRA->get(&Fn)->viewLoopRegions();
    }
  }
  StringRef getName() override { return "LoopRegionViewCFG"; }
};

} // end anonymous namespace

SILTransform *swift::createLoopRegionViewText() {
  return new LoopRegionViewText();
}

SILTransform *swift::createLoopRegionViewCFG() {
  return new LoopRegionViewCFG();
}
