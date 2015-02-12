//===-------- StripDebugInfo.cpp - Strip debug info from SIL --------------===//
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

#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILFunction.h"


using namespace swift;

static void stripFunction(SILFunction *F) {
  for (auto &BB : *F)
    for (auto II = BB.begin(), IE = BB.end(); II != IE;) {
      SILInstruction *Inst = II;
      ++II;

      if (!isa<DebugValueInst>(Inst) &&
          !isa<DebugValueAddrInst>(Inst))
        continue;

      Inst->eraseFromParent();
    }
}

namespace {
class StripDebugInfo : public swift::SILFunctionTransform {
  virtual ~StripDebugInfo() {}

  /// The entry point to the transformation.
  void run() override {
    stripFunction(getFunction());
    invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "Strip Debug Info"; }
};
} // end anonymous namespace


SILTransform *swift::createStripDebugInfo() {
  return new StripDebugInfo();
}
