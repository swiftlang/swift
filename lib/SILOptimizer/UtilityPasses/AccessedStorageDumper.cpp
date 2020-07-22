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
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

static void dumpAccessedStorage(SILInstruction *inst) {
  visitAccessedAddress(
    inst,
    [&](Operand *operand) {
      inst->print(llvm::outs());
      findAccessedStorage(operand->get()).print(llvm::outs());
    }
  );
}

namespace {

/// Dumps sorage information for each access.
class AccessedStorageDumper : public SILModuleTransform {

  void run() override {
    for (auto &fn : *getModule()) {
      llvm::outs() << "@" << fn.getName() << "\n";
      if (fn.empty()) {
        llvm::outs() << "<unknown>\n";
        continue;
      }
      for (auto &bb : fn) {
        for (auto &inst : bb) {
          if (inst.mayReadOrWriteMemory())
            dumpAccessedStorage(&inst);
        }
      }
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createAccessedStorageDumper() {
  return new AccessedStorageDumper();
}
