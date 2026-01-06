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
#include "swift/AST/DiagnosticGroups.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/Expr.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeVisitor.h"

using namespace swift;

bool swift::performanceHintDiagnosticsEnabled(ASTContext &ctx, SourceFile *sf) {
  return ctx.TypeCheckerOpts.SkipFunctionBodies == FunctionBodySkipping::None &&
         (ctx.Diags.isDiagnosticGroupEnabled(sf, DiagGroupID::PerformanceHints) ||
          ctx.Diags.isDiagnosticGroupEnabled(sf, DiagGroupID::ReturnTypeImplicitCopy) ||
          ctx.Diags.isDiagnosticGroupEnabled(sf, DiagGroupID::ExistentialType));
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

bool hasExistentialAnyInType(Type T) {
  return T->getCanonicalType().findIf(
      [](CanType CT) { return isa<ExistentialType>(CT); });
}

void checkExistentialInFunctionReturnType(const FuncDecl *FD,
                                          DiagnosticEngine &Diags) {
  Type T = FD->getResultInterfaceType();

  if (hasExistentialAnyInType(T))
    Diags.diagnose(FD, diag::perf_hint_func_returns_existential, FD);
}

void checkExistentialInClosureReturnType(const ClosureExpr *CE,
                                         DiagnosticEngine &Diags) {
  Type T = CE->getResultType();

  if (hasExistentialAnyInType(T))
    Diags.diagnose(CE->getLoc(), diag::perf_hint_closure_returns_existential);
}

void checkExistentialInVariableType(const VarDecl *VD,
                                    DiagnosticEngine &Diags) {
  Type T = VD->getInterfaceType();

  if (hasExistentialAnyInType(T))
    Diags.diagnose(VD, diag::perf_hint_var_uses_existential, VD);
}

void checkExistentialInPatternType(const AnyPattern *AP,
                                   DiagnosticEngine &Diags) {
  Type T = AP->getType();

  if (hasExistentialAnyInType(T))
    Diags.diagnose(AP->getLoc(), diag::perf_hint_any_pattern_uses_existential);
}

void checkExistentialInTypeAlias(const TypeAliasDecl *TAD,
                                 DiagnosticEngine &Diags) {
  Type T = TAD->getUnderlyingType();

  if (hasExistentialAnyInType(T))
    Diags.diagnose(TAD->getLoc(), diag::perf_hint_typealias_uses_existential,
                   TAD);
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

  PostWalkResult<Pattern *> walkToPatternPost(Pattern *P) override {
    if (P->isImplicit())
      return Action::Continue(P);

    if (const AnyPattern *AP = dyn_cast<AnyPattern>(P)) {
      checkExistentialInPatternType(AP, Ctx.Diags);
    }

    return Action::Continue(P);
  }

  PostWalkResult<Expr *> walkToExprPost(Expr *E) override {
    if (E->isImplicit())
      return Action::Continue(E);

    if (const ClosureExpr *CE = dyn_cast<ClosureExpr>(E)) {
      checkImplicitCopyReturnType(CE, Ctx.Diags);
      checkExistentialInClosureReturnType(CE, Ctx.Diags);
    }

    return Action::Continue(E);
  }

  PostWalkAction walkToDeclPost(Decl *D) override {
    if (D->isImplicit())
      return Action::Continue();

    if (const FuncDecl *FD = dyn_cast<FuncDecl>(D)) {
      checkImplicitCopyReturnType(FD, Ctx.Diags);
      checkExistentialInFunctionReturnType(FD, Ctx.Diags);
    } else if (const VarDecl *VD = dyn_cast<VarDecl>(D)) {
      checkExistentialInVariableType(VD, Ctx.Diags);
    } else if (const TypeAliasDecl *TAD = dyn_cast<TypeAliasDecl>(D)) {
      checkExistentialInTypeAlias(TAD, Ctx.Diags);
    }

    return Action::Continue();
  }
};
} // namespace

evaluator::SideEffect EmitPerformanceHints::evaluate(Evaluator &evaluator,
                                                     SourceFile *SF) const {
  PerformanceHintDiagnosticWalker::check(SF);
  return {};
}
