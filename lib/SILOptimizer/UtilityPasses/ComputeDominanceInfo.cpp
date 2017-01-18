//===--- ComputeDominanceInfo.cpp -----------------------------------------===//
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

#define DEBUG_TYPE "sil-compute-dominance-info"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"

using namespace swift;

class ComputeDominanceInfo : public SILFunctionTransform {

  void run() override {
    PM->getAnalysis<DominanceAnalysis>()->get(getFunction());
    PM->getAnalysis<PostDominanceAnalysis>()->get(getFunction());
  }

  StringRef getName() override { return "Compute Dominance Info"; }
};

SILTransform *swift::createComputeDominanceInfo() { return new ComputeDominanceInfo(); }
