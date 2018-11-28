//===--- ValueOwnershipKindDumper.cpp -------------------------------------===//
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
///
/// This is a simple utility pass that dumps the ValueOwnershipKind of all
/// SILValue in a module. It is meant to trigger assertions and verification of
/// these values.
///
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

static void checkEnumInstIsTrivial(EnumInst *EI) {
  if (!EI->hasOperand())
    return;

  if (EI->getOperand()->getType().isTrivial(EI->getModule()))
    return;

  llvm_unreachable("Found enum with non-trivial operand but trivial ownership?!");
}

namespace {

class ValueOwnershipKindDumper : public SILFunctionTransform {

  void run() override {
    SILFunction *F = getFunction();
    SILModule &M = F->getModule();
    for (auto &BB : *F) {
      // We only verify instructions right now.
      for (auto &II : BB) {
        // If the instruction doesn't have any results, bail.
        auto results = II.getResults();
        if (results.empty())
          continue;

        llvm::outs() << "Visiting: " << II;

        for (auto V : results) {
          auto Kind = V.getOwnershipKind();
          llvm::outs() << "    " << Kind << "\n";
          if (Kind == ValueOwnershipKind::Any)
            continue;

          if (V->getType().isTrivial(M)) {
            llvm_unreachable(
                "Error! Non Trivial ownership with trivial type\n");
          }
        }
      }
    }
  }

};

} // end anonymous namespace

SILTransform *swift::createValueOwnershipKindDumper() {
  return new ValueOwnershipKindDumper();
}
