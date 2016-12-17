//===--- ValueOwnershipKindDumper.cpp -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

namespace {

class ValueOwnershipKindDumper : public SILFunctionTransform {

  void run() override {
    SILFunction *F = getFunction();
    SILModule &M = F->getModule();
    for (auto &BB : *F) {
      // We only verify instructions right now.
      for (auto &II : BB) {
        // If the instruction doesn't have a value, bail.
        if (!II.hasValue())
          continue;
        SILValue V(&II);
        llvm::outs() << "Visiting: " << II;
        auto Kind = V.getOwnershipKind();
        if (!Kind.hasValue()) {
          llvm_unreachable("    Error... has a value but no kind?!");
        }
        llvm::outs() << "    " << Kind.getValue() << "\n";
        if (Kind.getValue() == ValueOwnershipKind::Trivial) {
          if (!V->getType().isTrivial(M)) {
            llvm_unreachable("Error! Trivial ownership without trivial type\n");
          }
        } else {
          if (V->getType().isTrivial(M)) {
            llvm_unreachable(
                "Error! Non Trivial ownership with trivial type\n");
          }
        }
      }
    }
  }

  StringRef getName() override { return "Value Ownership Kind Dumper"; }
};

} // end anonymous namespace

SILTransform *swift::createValueOwnershipKindDumper() {
  return new ValueOwnershipKindDumper();
}
