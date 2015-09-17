//===----- CallGraphPrinter.cpp - Call graph printing pass ----*- C++ -*---===//
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
// This pass prints the call graph for use in testing.
//
//===----------------------------------------------------------------------===//

#include "swift/SILAnalysis/CallGraphAnalysis.h"
#include "swift/SILPasses/Transforms.h"

using namespace swift;

#define DEBUG_TYPE "call-graph-printer"

namespace {

class CallGraphPrinterPass : public SILModuleTransform {
  /// The entry point to the transformation.
  void run() override {
    auto *CGA = getAnalysis<CallGraphAnalysis>();
    CallGraph &CG = CGA->getOrBuildCallGraph();
    CG.print(llvm::outs());
    CG.printStats(llvm::outs());
  }
  StringRef getName() override { return "Call Graph Printer"; }
};

} // end anonymous namespace

SILTransform *swift::createCallGraphPrinter() {
  return new CallGraphPrinterPass();
}
