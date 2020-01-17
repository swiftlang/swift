//===--- IRGenPrepare.cpp -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// Cleanup SIL to make it suitable for IRGen.
///
/// We perform the following canonicalizations:
///
/// 1. We remove calls to Builtin.poundAssert() and Builtin.staticReport(),
///    which are not needed post SIL.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-cleanup"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/Strings.h"

using namespace swift;

static bool cleanFunction(SILFunction &fn) {
  bool madeChange = false;

  for (auto &bb : fn) {
    for (auto i = bb.begin(), e = bb.end(); i != e;) {
      // Make sure there is no iterator invalidation if the inspected
      // instruction gets removed from the block.
      SILInstruction *inst = &*i;
      ++i;

      // Remove calls to Builtin.poundAssert() and Builtin.staticReport().
      auto *bi = dyn_cast<BuiltinInst>(inst);
      if (!bi) {
        continue;
      }

      switch (bi->getBuiltinInfo().ID) {
        case BuiltinValueKind::CondFailMessage: {
          SILBuilderWithScope Builder(bi);
          Builder.createCondFail(bi->getLoc(), bi->getOperand(0),
            "unknown program error");
          LLVM_FALLTHROUGH;
        }
        case BuiltinValueKind::PoundAssert:
        case BuiltinValueKind::StaticReport: {
          // The call to the builtin should get removed before we reach
          // IRGen.
          InstructionDeleter deleter;
          deleter.forceDelete(bi);
          // StaticReport only takes trivial operands, and therefore doesn't
          // require fixing the lifetime of its operands.
          deleter.cleanUpDeadInstructions();
          madeChange = true;
          break;
        }
        default:
          break;
      }
    }
  }

  return madeChange;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class IRGenPrepare : public SILFunctionTransform {
  void run() override {
    SILFunction *F = getFunction();

    bool shouldInvalidate = cleanFunction(*F);

    if (shouldInvalidate)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
};

} // end anonymous namespace


SILTransform *swift::createIRGenPrepare() {
  return new IRGenPrepare();
}
