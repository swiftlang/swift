//===--- MemBehaviorDumper.cpp --------------------------------------------===//
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

#define DEBUG_TYPE "sil-mem-behavior-dumper"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/SideEffectAnalysis.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                               Value Gatherer
//===----------------------------------------------------------------------===//

// Return a list of all instruction values in Fn. Returns true if we have at
// least two values to compare.
static bool gatherValues(SILFunction &Fn, std::vector<SILValue> &Values) {
  for (auto &BB : Fn) {
    for (auto *Arg : BB.getArguments())
      Values.push_back(SILValue(Arg));
    for (auto &II : BB)
      if (II.hasValue())
        Values.push_back(&II);
  }
  return Values.size() > 1;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

namespace {

/// Dumps the memory behavior of instructions in a function.
class MemBehaviorDumper : public SILModuleTransform {

  // To reduce the amount of output, we only dump the memory behavior of
  // selected types of instructions.
  static bool shouldTestInstruction(SILInstruction *I) {
    // Only consider function calls.
    if (FullApplySite::isa(I))
      return true;

    return false;
  }

  void run() override {
    for (auto &Fn : *getModule()) {
      llvm::outs() << "@" << Fn.getName() << "\n";
      // Gather up all Values in Fn.
      std::vector<SILValue> Values;
      if (!gatherValues(Fn, Values))
        continue;

      AliasAnalysis *AA = PM->getAnalysis<AliasAnalysis>();

      unsigned PairCount = 0;
      for (auto &BB : Fn) {
        for (auto &I : BB) {
          if (shouldTestInstruction(&I)) {

            // Print the memory behavior in relation to all other values in the
            // function.
            for (auto &V : Values) {
              bool Read = AA->mayReadFromMemory(&I, V);
              bool Write = AA->mayWriteToMemory(&I, V);
              bool SideEffects = AA->mayHaveSideEffects(&I, V);
              llvm::outs() << "PAIR #" << PairCount++ << ".\n"
                           << "  " << SILValue(&I) << "  " << V
                           << "  r=" << Read << ",w=" << Write
                           << ",se=" << SideEffects << "\n";
            }
          }
        }
      }
      llvm::outs() << "\n";
    }
  }

  StringRef getName() override { return "Memory Behavior Dumper"; }
};

} // end anonymous namespace

SILTransform *swift::createMemBehaviorDumper() {
  return new MemBehaviorDumper();
}
