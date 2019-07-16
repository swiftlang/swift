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
/// 2. We transform polymorphic builtins in transparent functions into traps.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-cleanup"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
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

      auto *bi = dyn_cast<BuiltinInst>(inst);
      if (!bi) {
        continue;
      }

      auto kind = bi->getBuiltinKind();
      if (!kind) {
        continue;
      }

      // Transform polymorphic builtins into int_trap.
      if (isPolymorphicBuiltin(*kind)) {
        assert(bi->getFunction()->isTransparent() == IsTransparent &&
               "Should only see these in transparent functions. If these were "
               "mandatory inlined into a caller, we should either have emitted "
               "a call to the static overload or emitted a diagnostic");
        // Replace all uses with undef since we are going to trap. Any such uses
        // are now unreachable along a path.
        bi->replaceAllUsesWithUndef();
        SILBuilderWithScope(bi).createBuiltinTrap(bi->getLoc());
        bi->eraseFromParent();
        madeChange = true;
        continue;
      }

      switch (*kind) {
      case BuiltinValueKind::CondFailMessage: {
        SILBuilderWithScope Builder(bi);
        Builder.createCondFail(bi->getLoc(), bi->getOperand(0),
                               "unknown program error");
        LLVM_FALLTHROUGH;
      }
      case BuiltinValueKind::PoundAssert:
      case BuiltinValueKind::StaticReport:
        // The call to the builtin should get removed before we reach
        // IRGen.
        recursivelyDeleteTriviallyDeadInstructions(bi, /* Force */ true);
        madeChange = true;
        break;
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
