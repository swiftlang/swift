//===--- AccessedStorageDumper.cpp - Dump accessed storage for functions ---===//
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

#define DEBUG_TYPE "sil-accessed-storage-dumper"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/AccessedStorageAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

/// Dumps per-function information on dynamically enforced formal accesses.
class AccessedStorageDumper : public SILModuleTransform {

  void run() override {
    auto *analysis = PM->getAnalysis<AccessedStorageAnalysis>();

    for (auto &fn : *getModule()) {
      llvm::outs() << "@" << fn.getName() << "\n";
      if (fn.empty()) {
        llvm::outs() << "<unknown>\n";
        continue;
      }
      const FunctionAccessedStorage &summary = analysis->getEffects(&fn);
      summary.print(llvm::outs());
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createAccessedStorageDumper() {
  return new AccessedStorageDumper();
}
