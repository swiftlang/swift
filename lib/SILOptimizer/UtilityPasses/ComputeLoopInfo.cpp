//===--- ComputeLoopInfo.cpp ----------------------------------------------===//
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

#define DEBUG_TYPE "sil-compute-loop-info"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"

using namespace swift;

class ComputeLoopInfo : public SILFunctionTransform {

  void run() override {
    PM->getAnalysis<SILLoopAnalysis>()->get(getFunction());
  }

  StringRef getName() override { return "Compute Loop Info"; }
};

SILTransform *swift::createComputeLoopInfo() { return new ComputeLoopInfo(); }
