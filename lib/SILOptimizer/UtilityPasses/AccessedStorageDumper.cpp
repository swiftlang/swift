//===--- AccessedStorageDumper.cpp - Dump accessed storage ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
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
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

using namespace swift;

static llvm::cl::opt<bool> EnableDumpUses(
    "enable-accessed-storage-dump-uses", llvm::cl::init(false),
    llvm::cl::desc("With --sil-access-storage-dumper, dump all uses"));

namespace {

/// Dumps sorage information for each access.
class AccessedStorageDumper : public SILModuleTransform {
  llvm::SmallVector<Operand *, 32> uses;

  void dumpAccessedStorage(Operand *operand) {
    findAccessedStorage(operand->get()).print(llvm::outs());
    auto pathAndBase = AccessPathWithBase::compute(operand->get());
    pathAndBase.print(llvm::outs());

    if (!pathAndBase.accessPath.isValid() || !EnableDumpUses)
      return;

    uses.clear();
    pathAndBase.collectUses(uses, /*collectContainingUses*/ false);
    llvm::outs() << "Exact Uses {\n";
    for (auto *useOperand : uses) {
      llvm::outs() << *useOperand->getUser() << "  ";
      auto usePathAndBase = AccessPathWithBase::compute(useOperand->get());
      usePathAndBase.accessPath.printPath(llvm::outs());
      assert(pathAndBase.accessPath.contains(usePathAndBase.accessPath)
             && "access path does not contain use access path");
    }
    llvm::outs() << "}\n";
    uses.clear();
    pathAndBase.collectUses(uses, /*collectContainingUses*/ true);
    llvm::outs() << "Overlapping Uses {\n";
    for (auto *useOperand : uses) {
      llvm::outs() << *useOperand->getUser() << "  ";
      auto usePathAndBase = AccessPathWithBase::compute(useOperand->get());
      usePathAndBase.accessPath.printPath(llvm::outs());
      assert(pathAndBase.accessPath.mayOverlap(usePathAndBase.accessPath)
             && "access path does not contain use access path");
    }
    llvm::outs() << "}\n";
  }

  void run() override {
    for (auto &fn : *getModule()) {
      llvm::outs() << "@" << fn.getName() << "\n";
      if (fn.empty()) {
        llvm::outs() << "<unknown>\n";
        continue;
      }
      for (auto &bb : fn) {
        for (auto &inst : bb) {
          if (inst.mayReadOrWriteMemory()) {
            llvm::outs() << "###For MemOp: " << inst;
            visitAccessedAddress(&inst, [this](Operand *operand) {
              dumpAccessedStorage(operand);
            });
          }
        }
      }
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createAccessedStorageDumper() {
  return new AccessedStorageDumper();
}
