//===--- VerifyOwnership.cpp - Verify OSSA lifetimes ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// Run ownership verification and the Swift verifier on each OSSA function,
/// including in compilers built without assertions, where the C++ SIL verifier
/// does not run by default.
///
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

namespace {

class VerifyOwnership : public SILFunctionTransform {
  void run() override {
    SILFunction *f = getFunction();
    if (!f->hasOwnership() || f->getModule().getOptions().VerifyNone)
      return;

    getPassManager()->runMandatoryVerification(f);
  }
};

} // end anonymous namespace

SILTransform *swift::createVerifyOwnership() { return new VerifyOwnership(); }
