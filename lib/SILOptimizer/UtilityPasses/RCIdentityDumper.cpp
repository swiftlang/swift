//===--- RCIdentityDumper.cpp ---------------------------------------------===//
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
///
/// This pass applies the RCIdentityAnalysis to all SILValues in a function in
/// order to apply FileCheck testing to RCIdentityAnalysis without needing to
/// test any other passes.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-rc-identity-dumper"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

/// Dumps the alias relations between all instructions of a function.
class RCIdentityDumper : public SILFunctionTransform {

  void run() override {
    auto *Fn = getFunction();
    auto *RCId = PM->getAnalysis<RCIdentityAnalysis>()->get(Fn);

    std::vector<std::pair<SILValue, SILValue>> Results;
    unsigned ValueCount = 0;
    llvm::MapVector<SILValue, uint64_t> ValueToValueIDMap;

    llvm::outs() << "@" << Fn->getName() << "@\n";

    for (auto &BB : *Fn) {
      for (auto *Arg : BB.getArguments()) {
        ValueToValueIDMap[Arg] = ValueCount++;
        Results.push_back({Arg, RCId->getRCIdentityRoot(Arg)});
      }
      for (auto &II : BB) {
        if (II.hasValue()) {
          SILValue V(&II);
          ValueToValueIDMap[V] = ValueCount++;
          Results.push_back({V, RCId->getRCIdentityRoot(V)});
        }
      }
    }

    llvm::outs() << "ValueMap:\n";
    for (auto P : ValueToValueIDMap) {
      llvm::outs() << "\tValueMap[" << P.second << "] = " << P.first;
    }

    unsigned ResultCount = 0;
    for (auto P : Results) {
      llvm::outs() << "RESULT #" << ResultCount++ << ": "
                   << ValueToValueIDMap[P.first] << " = "
                   << ValueToValueIDMap[P.second] << "\n";
    }

    llvm::outs() << "\n";
  }

  StringRef getName() override { return "RC Identity Dumper"; }
};

} // end anonymous namespace

SILTransform *swift::createRCIdentityDumper() { return new RCIdentityDumper(); }
