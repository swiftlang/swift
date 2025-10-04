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
#include "swift/AST/TypeVisitor.h"

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

class HasExistentialAnyType : public TypeVisitor<HasExistentialAnyType, bool> {
public:
  static bool check(Type type) {
    return HasExistentialAnyType().visit(type->getCanonicalType());
  }

  bool visitExistentialType(ExistentialType *ET) {
    return true;
  }

  bool visitTupleType(TupleType *TT) {
    for (const auto &element : TT->getElements()) {
      if (visit(element.getType())) {
        return true;
      }
    }
    return false;
  }

  bool visitBoundGenericType(BoundGenericType *BGT) {
    // Check generic arguments (e.g., Array<any Protocol>)
    for (Type arg : BGT->getGenericArgs()) {
      if (visit(arg)) {
        return true;
      }
    }
    return false;
  }

  bool visitFunctionType(FunctionType *FT) {
    for (const auto &param : FT->getParams()) {
      if (visit(param.getPlainType()->getCanonicalType())) {
        return true;
      }
    }

    return visit(FT->getResult()->getCanonicalType());
  }

  bool visitType(TypeBase *T) {
    return false;
  }
};

void checkExistentialAnyReturnType(FuncDecl *FD, DiagnosticEngine &Diags) {
  Type T = FD->getResultInterfaceType();

  if (HasExistentialAnyType::check(T))
    Diags.diagnose(FD, diag::perf_hint_func_returns_existential, FD);
}

void checkExistentialAnyReturnType(ClosureExpr *CE, DiagnosticEngine &Diags) {
  Type T = CE->getResultType();

  if (HasExistentialAnyType::check(T))
    Diags.diagnose(CE->getLoc(), diag::perf_hint_closure_returns_existential);
}

void checkExistentialAnyVarType(const VarDecl *VD, DiagnosticEngine &Diags) {
  Type T = VD->getInterfaceType();

  if (HasExistentialAnyType::check(T))
    Diags.diagnose(VD, diag::perf_hint_var_uses_existential, VD);
}

void checkExistentialAnyPatternType(const AnyPattern *AP,
                                    DiagnosticEngine &Diags) {
  Type T = AP->getType();

  if (HasExistentialAnyType::check(T))
    Diags.diagnose(AP->getLoc(), diag::perf_hint_pat_uses_existential);
}

void checkExistentialAnyTypeAlias(const TypeAliasDecl *TAD,
                                  DiagnosticEngine &Diags) {
  Type T = TAD->getUnderlyingType();

  if (HasExistentialAnyType::check(T))
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

  PreWalkResult<Pattern *> walkToPatternPre(Pattern *P) override {
    if (P->isImplicit())
      return Action::SkipNode(P);

    if (const AnyPattern *AP = dyn_cast<AnyPattern>(P)) {
      checkExistentialAnyPatternType(AP, Ctx.Diags);
    }

    return Action::Continue(P);
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    if (E->isImplicit())
      return Action::SkipNode(E);

    if (const ClosureExpr* Closure = dyn_cast<ClosureExpr>(E)) {
      checkImplicitCopyReturnType(Closure, Ctx.Diags);
    }

    return Action::Continue(E);
  }

  PostWalkResult<Expr *> walkToExprPost(Expr *E) override {
    assert(
        !E->isImplicit() &&
        "Traversing implicit expressions is disabled in the pre-walk visitor");

    if (auto Closure = dyn_cast<ClosureExpr>(E)) {
      checkExistentialAnyReturnType(Closure, Ctx.Diags);
    }

    return Action::Continue(E);
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    if (D->isImplicit())
      return Action::SkipNode();

    if (const FuncDecl *FD = dyn_cast<FuncDecl>(D)) {
      checkImplicitCopyReturnType(FD, Ctx.Diags);
    } else if (const VarDecl *VD = dyn_cast<VarDecl>(D)) {
      checkExistentialAnyVarType(VD, Ctx.Diags);
    } else if (const TypeAliasDecl *TAD = dyn_cast<TypeAliasDecl>(D)) {
      checkExistentialAnyTypeAlias(TAD, Ctx.Diags);
    }

    return Action::Continue();
  }

  PostWalkAction walkToDeclPost(Decl *D) override {
    assert(
        !D->isImplicit() &&
        "Traversing implicit declarations is disabled in the pre-walk visitor");

    if (auto *FD = dyn_cast<FuncDecl>(D)) {
      checkExistentialAnyReturnType(FD, Ctx.Diags);
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
