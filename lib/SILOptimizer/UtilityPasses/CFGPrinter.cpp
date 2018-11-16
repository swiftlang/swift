//===--- CFGPrinter.cpp - CFG printer pass --------------------------------===//
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
// This file defines external functions that can be called to explicitly
// instantiate the CFG printer.
//
//===----------------------------------------------------------------------===//

#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                  Options
//===----------------------------------------------------------------------===//

llvm::cl::opt<std::string> SILViewCFGOnlyFun(
    "sil-view-cfg-only-function", llvm::cl::init(""),
    llvm::cl::desc("Only produce a graphviz file for this function"));

llvm::cl::opt<std::string> SILViewCFGOnlyFuns(
    "sil-view-cfg-only-functions", llvm::cl::init(""),
    llvm::cl::desc("Only produce a graphviz file for the sil for the functions "
                   "whose name contains this substring"));

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

namespace {

class SILCFGPrinter : public SILFunctionTransform {
  /// The entry point to the transformation.
  void run() override {
    SILFunction *F = getFunction();

    // If we are not supposed to dump view this cfg, return.
    if (!SILViewCFGOnlyFun.empty() && F && F->getName() != SILViewCFGOnlyFun)
      return;
    if (!SILViewCFGOnlyFuns.empty() && F &&
        F->getName().find(SILViewCFGOnlyFuns, 0) == StringRef::npos)
      return;

    F->viewCFG();
  }
};

} // end anonymous namespace

SILTransform *swift::createCFGPrinter() {
  return new SILCFGPrinter();
}
