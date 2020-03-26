//===--- BasicCalleePrinter.cpp - Callee cache printing pass --------------===//
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
// This pass prints the callees of functions as determined by the
// BasicCalleeAnalysis. The pass is strictly for testing that
// analysis.
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

#define DEBUG_TYPE "basic-callee-printer"

namespace {

class BasicCalleePrinterPass : public SILModuleTransform {
  BasicCalleeAnalysis *BCA;

  void printCallees(FullApplySite FAS) {
    llvm::outs() << "Function call site:\n";
    if (auto *Callee = FAS.getCallee()->getDefiningInstruction())
      llvm::outs() << *Callee;
    llvm::outs() << *FAS.getInstruction();

    auto Callees = BCA->getCalleeList(FAS);
    Callees.print(llvm::outs());
  }

  /// The entry point to the transformation.
  void run() override {
    BCA = getAnalysis<BasicCalleeAnalysis>();
    for (auto &Fn : *getModule()) {
      if (Fn.isExternalDeclaration()) continue;
      for (auto &B : Fn)
        for (auto &I : B)
          if (auto FAS = FullApplySite::isa(&I))
            printCallees(FAS);
    }
  }

};

} // end anonymous namespace

SILTransform *swift::createBasicCalleePrinter() {
  return new BasicCalleePrinterPass();
}
