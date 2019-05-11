//===-----ConstantEvaluatorTester.cpp - Test Constant Evaluator -----------===//
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

#define DEBUG_TYPE "sil-constant-evaluation-tester"
#include "swift/SIL/SILConstants.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/ConstExpr.h"

using namespace swift;

namespace {

/// A compiler pass for testing constant evaluator in the step-wise evaluation
/// mode. The pass evaluates SIL functions whose names start with "interpret"
/// and outputs the constant value returned by the function or diagnostics if
/// the evaluation fails.
class ConstantEvaluatorTester : public SILFunctionTransform {

  bool shouldInterpret() {
    auto *fun = getFunction();
    return fun->getName().startswith("interpret");
  }

  void run() override {
    SILFunction *fun = getFunction();

    if (!shouldInterpret() || fun->empty())
      return;

    llvm::errs() << "@" << fun->getName() << "\n";

    SymbolicValueBumpAllocator allocator;
    ConstExprStepEvaluator stepEvaluator(allocator, fun);

    for (auto currI = fun->getEntryBlock()->begin();;) {
      auto *inst = &(*currI);

      if (auto *returnInst = dyn_cast<ReturnInst>(inst)) {
        auto returnVal =
            stepEvaluator.lookupConstValue(returnInst->getOperand());

        if (!returnVal) {
          llvm::errs() << "Returns unknown"
                       << "\n";
        }
        llvm::errs() << "Returns " << returnVal.getValue() << "\n";
        break;
      }

      Optional<SILBasicBlock::iterator> nextInstOpt;
      Optional<SymbolicValue> errorVal;

      std::tie(nextInstOpt, errorVal) = stepEvaluator.evaluate(currI);
      if (errorVal.hasValue()) {
        // Diagnose the error.
        assert(errorVal->getKind() == SymbolicValue::Unknown);
        errorVal->emitUnknownDiagnosticNotes(inst->getLoc());
        break;
      }

      assert(nextInstOpt);
      currI = nextInstOpt.getValue();
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createConstantEvaluatorTester() {
  return new ConstantEvaluatorTester();
}
