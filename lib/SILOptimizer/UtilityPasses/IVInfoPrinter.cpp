//===--- IVInfoPrinter.cpp - Print SIL IV Info ----------------------------===//
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

#include "swift/SILOptimizer/Analysis/IVAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

namespace {

class IVInfoPrinter : public SILModuleTransform {

  StringRef getName() override { return "SIL IV Information Printer"; }

  void dumpIV(ValueBase *Header, ValueBase *IV) {
    if (IV == Header) {
      llvm::errs() << "IV Header: ";
      IV->dump();
      return;
    }

    llvm::errs() << "IV: ";
    IV->dump();
    llvm::errs() << "with header: ";
    Header->dump();
  }

  /// The entry point to the transformation.
  void run() override {
    auto *IV = PM->getAnalysis<IVAnalysis>();

    for (auto &F : *getModule()) {
      if (F.isExternalDeclaration()) continue;

      auto &Info = *IV->get(&F);

      bool FoundIV = false;

      for (auto &BB : F) {
        for (auto A : BB.getArguments())
          if (Info.isInductionVariable(A)) {
            if (!FoundIV)
              llvm::errs() << "Induction variables for function: " <<
              F.getName() << "\n";

            FoundIV = true;
            dumpIV(Info.getInductionVariableHeader(A), A);
          }

        for (auto &I : BB)
          if (Info.isInductionVariable(&I)) {
            if (!FoundIV)
              llvm::errs() << "Induction variables for function: " <<
              F.getName() << "\n";

            FoundIV = true;
            dumpIV(Info.getInductionVariableHeader(&I), &I);
          }
      }
      
      if (FoundIV)
        llvm::errs() << "\n";
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createIVInfoPrinter() {
  return new IVInfoPrinter();
}
