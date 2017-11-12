//===--- AccessSummaryDumper.cpp - Dump access summaries for functions -----===//
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

#define DEBUG_TYPE "sil-access-summary-dumper"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/AccessSummaryAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

/// Dumps summaries of kinds of accesses a function performs on its
/// @inout_aliasiable arguments.
class AccessSummaryDumper : public SILModuleTransform {

  void run() override {
    auto *analysis = PM->getAnalysis<AccessSummaryAnalysis>();

    for (auto &fn : *getModule()) {
      llvm::outs() << "@" << fn.getName() << "\n";
      if (fn.empty()) {
        llvm::outs() << "<unknown>\n";
        continue;
      }
      const AccessSummaryAnalysis::FunctionSummary &summary =
          analysis->getOrCreateSummary(&fn);
      summary.print(llvm::outs(), &fn);
      llvm::outs() << "\n";
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createAccessSummaryDumper() {
  return new AccessSummaryDumper();
}
