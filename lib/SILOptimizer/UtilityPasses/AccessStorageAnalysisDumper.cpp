//===--- AccessStorageAnalysisDumper.cpp - accessed storage anlaysis ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-accessed-storage-analysys-dumper"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/AccessStorageAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

/// Dumps per-function information on dynamically enforced formal accesses.
class AccessStorageAnalysisDumper : public SILModuleTransform {

  void run() override {
    auto *analysis = PM->getAnalysis<AccessStorageAnalysis>();

    for (auto &fn : *getModule()) {
      llvm::outs() << "@" << fn.getName() << "\n";
      if (fn.empty()) {
        llvm::outs() << "<unknown>\n";
        continue;
      }
      const FunctionAccessStorage &summary = analysis->getEffects(&fn);
      summary.print(llvm::outs());
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createAccessStorageAnalysisDumper() {
  return new AccessStorageAnalysisDumper();
}
