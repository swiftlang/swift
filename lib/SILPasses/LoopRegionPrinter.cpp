//===--- LoopRegionPrinter.cpp --------------------------------------------===//
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
//
// Simple pass for testing the new loop region dumper analysis. Prints out
// information suitable for checking with filecheck.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-loop-region-printer"

#include "swift/SILAnalysis/LoopRegionAnalysis.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"

using namespace swift;

namespace {

class LoopRegionViewText : public SILFunctionTransform {
  void run() override {
    auto *Fn = getFunction();
    llvm::outs() << "@" << Fn->getName() << "@\n";
    PM->getAnalysis<LoopRegionAnalysis>()->get(Fn)->dump();
    llvm::outs() << "\n";
    llvm::outs().flush();
  }

  virtual StringRef getName() override { return "LoopRegionViewText"; }
};

class LoopRegionViewCFG : public SILFunctionTransform {
  void run() override {
    auto *Fn = getFunction();
    PM->getAnalysis<LoopRegionAnalysis>()->get(Fn)->viewLoopRegions();
  }
  virtual StringRef getName() override { return "LoopRegionViewCFG"; }
};

} // end anonymous namespace

SILTransform *swift::createLoopRegionViewText() {
  return new LoopRegionViewText();
}

SILTransform *swift::createLoopRegionViewCFG() {
  return new LoopRegionViewCFG();
}
