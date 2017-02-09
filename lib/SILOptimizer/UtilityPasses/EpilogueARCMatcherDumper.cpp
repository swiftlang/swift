//===--- EpilogueARCMatcherDumper.cpp - Find Epilogue Releases ------------===//
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
/// \file
/// This pass finds the epilogue releases matched to each argument of the
/// function.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-epilogue-arc-dumper"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/EpilogueARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

namespace {

/// Find and dump the epilogue release instructions for the arguments.
class SILEpilogueARCMatcherDumper : public SILModuleTransform {
  void run() override {
    for (auto &F: *getModule()) {
      // Function is not definition.
      if (!F.isDefinition())
        continue;

      // Find the epilogue releases of each owned argument. 
      for (auto Arg : F.getArguments()) {
        auto *EA = PM->getAnalysis<EpilogueARCAnalysis>()->get(&F);
        llvm::outs() <<"START: " <<  F.getName() << "\n";
        llvm::outs() << *Arg;

        // Find the retain instructions for the argument.
        llvm::SmallSetVector<SILInstruction *, 1> RelInsts = 
          EA->computeEpilogueARCInstructions(EpilogueARCContext::EpilogueARCKind::Release,
                                             Arg);
        for (auto I : RelInsts) {
          llvm::outs() << *I << "\n";
        }

        // Find the release instructions for the argument.
        llvm::SmallSetVector<SILInstruction *, 1> RetInsts = 
          EA->computeEpilogueARCInstructions(EpilogueARCContext::EpilogueARCKind::Retain,
                                             Arg);
        for (auto I : RetInsts) {
          llvm::outs() << *I << "\n";
        }

        llvm::outs() <<"FINISH: " <<  F.getName() << "\n";
      }
    }
  }

  StringRef getName() override { return "Epilogue ARC Matcher Dumper"; }
};
        
} // end anonymous namespace

SILTransform *swift::createEpilogueARCMatcherDumper() {
  return new SILEpilogueARCMatcherDumper();
}
