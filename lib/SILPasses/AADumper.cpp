//===-------- AADumper.cpp - Compare all values in Function with AA -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This pass collects all values in a function and applies alias analysis to
// them. The purpose of this is to enable unit tests for SIL Alias Analysis
// implementations independent of any other passes.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-aa-evaluator"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                               Value Gatherer
//===----------------------------------------------------------------------===//

// Return a list of all instruction values in Fn. Returns true if we have at
// least two values to compare.
static bool gatherValues(SILFunction &Fn, std::vector<SILValue> &Values) {
  for (auto &BB : Fn) {
    for (auto *Arg : BB.getBBArgs())
      Values.push_back(SILValue(Arg));
    for (auto &II : BB)
      for (unsigned i = 0, e = II.getNumTypes(); i != e; ++i)
        Values.push_back(SILValue(&II, i));
  }
  return Values.size() > 1;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

namespace {

class SILAADumper : public SILFunctionTransform {
  
  void run() {
    SILFunction &Fn = *getFunction();
    llvm::outs() << "@" << Fn.getName() << "\n";
    // Gather up all Values in Fn.
    std::vector<SILValue> Values;
    if (!gatherValues(Fn, Values))
      return;

    AliasAnalysis *AA = PM->getAnalysis<AliasAnalysis>();

    // Emit the N^2 alias evaluation of the values.
    unsigned PairCount = 0;
    for (auto V1 : Values)
      for (auto V2 : Values)
        llvm::outs() << "PAIR #" << PairCount++ << ".\n" << V1 << V2 
                     << AA->alias(V1, V2) << "\n";
  }
};

} // end anonymous namespace

SILTransform *swift::createSILAADumper() {
  return new SILAADumper();
}


