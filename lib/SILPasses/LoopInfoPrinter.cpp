//===------------ LoopInfoPrinter.h - Print SIL Loop Info -*- C++ -*-------===//
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

#include "swift/SILAnalysis/SILLoopInfo.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"
#include "llvm/ADT/Statistic.h"

using namespace swift;

namespace {

class LoopInfoPrinter : public SILFunctionTransform {

  StringRef getName() override { return "SIL Loop Information Printer"; }

  /// The entry point to the transformation.
  void run() override {
    SILLoopAnalysis *LA = PM->getAnalysis<SILLoopAnalysis>();
    assert(LA);
    SILFunction *F = getFunction();
    assert(F);
    SILLoopInfo *LI = LA->getLoopInfo(F);
    assert(LI);

    if (LI->empty()) {
      llvm::errs() << "No loops in " << F->getName() << "\n";
      return;
    }

    llvm::errs() << "Loops in " << F->getName() << "\n";
    for (auto *LoopIt : *LI) {
      LoopIt->dump();
    }
  }
};

} // end anonymous namespace


SILTransform *swift::createLoopInfoPrinter() {
  return new LoopInfoPrinter();
}

