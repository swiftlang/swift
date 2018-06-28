//===--- DiagnoseCompilerEvaluable.cpp - Check @compilerEvaluable decls ---===//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// SWIFT_ENABLE_TENSORFLOW
// Checks that @compilerEvaluable decls follow all rules for what is allowed to
// execute at compile time.
//
//===----------------------------------------------------------------------===//

#include "TFConstExpr.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

class DiagnoseCompilerEvaluable : public SILFunctionTransform {
public:
  DiagnoseCompilerEvaluable() {}

private:
  void run() override {
    auto &M = getFunction()->getModule();

    if (!getFunction()->getCompilerEvaluableAttr())
      return;

    tf::ConstExprEvaluator evaluator(M);
    auto result = evaluator.checkCompilerEvaluable(*getFunction());

    if (result) {
      M.getASTContext().Diags.diagnose(getFunction()->getLocation().getSourceLoc(), diag::compiler_evaluable_bad_operation);
      result->emitUnknownDiagnosticNotes(getFunction()->getLocation());
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createDiagnoseCompilerEvaluable() {
  return new DiagnoseCompilerEvaluable();
}
