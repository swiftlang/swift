//===--------------------- PerformanceHints.cpp ---------------------------===//
//
// This source file is part of the Swift.org open source project

//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This files implements the various checks/lints intended to provide
// opt-in guidance for hidden costs in performance-critical Swift code.
//
//===----------------------------------------------------------------------===//

#include "MiscDiagnostics.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/Expr.h"
#include "swift/AST/TypeCheckRequests.h"

using namespace swift;

bool swift::performanceHintDiagnosticsEnabled(ASTContext &ctx) {
  return !ctx.Diags.isIgnoredDiagnostic(diag::perf_hint_closure_returns_array.ID) ||
         !ctx.Diags.isIgnoredDiagnostic(diag::perf_hint_function_returns_array.ID);
}

namespace {

void checkImplicitCopyReturnType(const FuncDecl *FD, DiagnosticEngine &Diags) {
  auto ReturnType = FD->getResultInterfaceType();
  if (ReturnType->isArray() || ReturnType->isDictionary()) {
    Diags.diagnose(FD->getLoc(), diag::perf_hint_function_returns_array, FD,
                   ReturnType->isArray());
  }
}

void checkImplicitCopyReturnType(const ClosureExpr *Closure,
                                 DiagnosticEngine &Diags) {
  auto ReturnType = Closure->getResultType();
  if (ReturnType->isArray() || ReturnType->isDictionary()) {
    Diags.diagnose(Closure->getLoc(), diag::perf_hint_closure_returns_array,
                   ReturnType->isArray());
  }
}

/// Produce performance hint diagnostics for a SourceFile.
class PerformanceHintDiagnosticWalker final : public ASTWalker {
  ASTContext &Ctx;

public:
  PerformanceHintDiagnosticWalker(ASTContext &Ctx) : Ctx(Ctx) {}

  static void check(SourceFile *SF) {
    auto Walker = PerformanceHintDiagnosticWalker(SF->getASTContext());
    SF->walk(Walker);
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    if (auto Closure = dyn_cast<ClosureExpr>(E))
      checkImplicitCopyReturnType(Closure, Ctx.Diags);

    return Action::Continue(E);
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    if (auto *FD = dyn_cast<FuncDecl>(D))
      checkImplicitCopyReturnType(FD, Ctx.Diags);

    return Action::Continue();
  }
};
} // namespace

evaluator::SideEffect EmitPerformanceHints::evaluate(Evaluator &evaluator,
                                                     SourceFile *SF) const {
  PerformanceHintDiagnosticWalker::check(SF);
  return {};
}
