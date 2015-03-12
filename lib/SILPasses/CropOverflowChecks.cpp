//===-- CropOverflowChecks.cpp - Delete masked overflow checkes -*- C++ -*-===//
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
// Remove overflow checks that are guarded by control flow or other
// overflow checks.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "crop-overflow-checks"

#include "swift/SIL/Dominance.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

class CropOverflowChecksPass : public SILFunctionTransform {
public:
  CropOverflowChecksPass() {}

  void run() override {
  }

  StringRef getName() override { return "Removes overflow checks that are proven to be redundant"; }
};
}

SILTransform *swift::createCropOverflowChecks() {
  return new CropOverflowChecksPass();
}
