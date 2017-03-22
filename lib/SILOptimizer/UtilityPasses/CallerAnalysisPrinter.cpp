//===--- CallerAnalysisPrinter.cpp - Caller Analysis test pass ------------===//
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
// This pass prints all the callsites of every function in the module.
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/Analysis/CallerAnalysis.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

#define DEBUG_TYPE "caller-analysis-printer"

namespace {

class CallerAnalysisPrinterPass : public SILModuleTransform {
  /// The entry point to the transformation.
  void run() override {
    auto *CA = getAnalysis<CallerAnalysis>();
    for (auto &F : *getModule()) {
      const CallerAnalysis::FunctionInfo &FI = CA->getCallerInfo(&F);
      const char *hasCaller = FI.hasCaller() ? "true" : "false";
      llvm::outs() << "Function " << F.getName() << " has caller: "
                   << hasCaller << ", partial applied args = "
                   << FI.getMinPartialAppliedArgs() << "\n";
    }
  }

  StringRef getName() override { return "Caller Analysis Printer"; }
};

} // end anonymous namespace

SILTransform *swift::createCallerAnalysisPrinter() {
  return new CallerAnalysisPrinterPass();
}
