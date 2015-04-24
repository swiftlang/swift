//===--- TypeCheckError.cpp - Type Checking for Error Coverage ------------===//
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
//
// This file implements semantic analysis to ensure that errors are
// caught.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsSema.h"

using namespace swift;

namespace {
/// An error-handling context.
class Context {
public:
  enum class Kind : uint8_t {
    /// A context that handles errors.
    Handled,

    /// A non-throwing function.
    NonThrowingFunction,

    /// A non-throwing autoclosure.
    NonThrowingAutoClosure,

    /// A non-exhaustive catch within a non-throwing function.
    NonExhaustiveCatch,

    /// The initializer for an instance variable.
    IVarInitializer,

    /// The initializer for a global variable.
    GlobalVarInitializer,

    /// The pattern of a catch.
    CatchPattern,

    /// The pattern of a catch.
    CatchGuard,
  };

private:
  template <class T>
  static Kind getKindForFunctionBody(T *fn) {
    return (fn->isBodyThrowing() ? Kind::Handled : Kind::NonThrowingFunction);
  }

  Kind TheKind;

  Context(Kind kind) : TheKind(kind) {}

public:
  static Context getHandled() {
    return Context(Kind::Handled);
  }

  static Context forClosure(AbstractClosureExpr *E) {
    auto kind = getKindForFunctionBody(E);
    if (kind != Kind::Handled && isa<AutoClosureExpr>(E))
      kind = Kind::NonThrowingAutoClosure;
    return Context(kind);
  }

  static Context forNonExhaustiveCatch(DoCatchStmt *S) {
    return Context(Kind::NonExhaustiveCatch);
  }

  static Context forCatchPattern(CatchStmt *S) {
    return Context(Kind::CatchPattern);
  }

  static Context forCatchGuard(CatchStmt *S) {
    return Context(Kind::CatchGuard);
  }

  Kind getKind() const { return TheKind; }
  bool isHandled() const { return getKind() == Kind::Handled; }

  static void diagnoseThrowInIllegalContext(TypeChecker &TC, Expr *E,
                                            StringRef description) {
    if (isa<ApplyExpr>(E)) {
      TC.diagnose(E->getLoc(), diag::throwing_call_in_illegal_context,
                  description);
    } else {
      TC.diagnose(E->getLoc(), diag::throw_in_illegal_context, description);
    }
  }

  void diagnoseUnhandledThrowSite(TypeChecker &TC, Expr *E, bool isTryCovered) {
    assert(isa<ThrowExpr>(E) || isa<ApplyExpr>(E));

    switch (getKind()) {
    case Kind::Handled:
      llvm_unreachable("throw site is handled!");

    // TODO: Doug suggested that we could generate one error per
    // non-throwing function with throw sites within it, possibly with
    // notes for the throw sites.

    case Kind::NonThrowingFunction:
      if (isa<ThrowExpr>(E)) {
        TC.diagnose(E->getLoc(), diag::throw_in_nonthrowing_function);
      } else if (isTryCovered) {
        // Allow the diagnostic to fire on the 'try'.
      } else {
        TC.diagnose(E->getLoc(), diag::tryless_throwing_call_unhandled);
      }
      return;

    case Kind::NonThrowingAutoClosure:
      if (isa<ThrowExpr>(E)) {
        TC.diagnose(E->getLoc(), diag::throw_in_nonthrowing_autoclosure);
      } else if (isTryCovered) {
        TC.diagnose(E->getLoc(),
                    diag::throwing_call_in_nonthrowing_autoclosure);
      } else {
        TC.diagnose(E->getLoc(),
                    diag::tryless_throwing_call_in_nonthrowing_autoclosure);
      }
      return;

    case Kind::NonExhaustiveCatch:
      if (isa<ThrowExpr>(E)) {
        TC.diagnose(E->getLoc(), diag::throw_in_nonexhaustive_catch);
      } else if (isTryCovered) {
        // Allow the diagnostic to fire on the 'try'.
      } else {
        TC.diagnose(E->getLoc(),
                    diag::tryless_throwing_call_in_nonexhaustive_catch);
      }
      return;

    case Kind::GlobalVarInitializer:
      diagnoseThrowInIllegalContext(TC, E, "a global variable initializer");
      return;

    case Kind::IVarInitializer:
      diagnoseThrowInIllegalContext(TC, E, "a property initializer");
      return;

    case Kind::CatchPattern:
      diagnoseThrowInIllegalContext(TC, E, "a catch pattern");
      return;

    case Kind::CatchGuard:
      diagnoseThrowInIllegalContext(TC, E, "a catch guard expression");
      return;
    }
    llvm_unreachable("bad context kind");
  }

  void diagnoseUnhandledTry(TypeChecker &TC, TryExpr *E) {
    switch (getKind()) {
    case Kind::Handled:
      llvm_unreachable("try is handled!");

    case Kind::NonThrowingFunction:
      TC.diagnose(E->getLoc(), diag::try_unhandled);
      return;

    case Kind::NonThrowingAutoClosure:
      // Diagnosed at the call sites.
      return;

    case Kind::NonExhaustiveCatch:
      TC.diagnose(E->getLoc(), diag::try_unhandled_in_nonexhaustive_catch);
      return;

    case Kind::GlobalVarInitializer:
    case Kind::IVarInitializer:
    case Kind::CatchPattern:
    case Kind::CatchGuard:
      // Diagnosed at the call sites.
      return;
    }
    llvm_unreachable("bad context kind");
  }
};

/// A class to walk over a local context and validate the correct of 
class CheckErrorCoverage : public ASTWalker {
  TypeChecker &TC;

  Context CurContext;

  /// Are we currently within a 'try'?
  bool IsInTry = false;

  /// Do we have any throw site in this context?
  bool HasAnyThrowSite = false;

  /// Do we have a throw site using 'try' in this context?
  bool HasTryThrowSite = false;

  /// An RAII object for restoring all the interesting state in an
  /// error-coverage.
  class ContextScope {
    CheckErrorCoverage &Self;
    Context OldContext;
    bool OldIsInTry;
    bool OldHasAnyThrowSite;
    bool OldHasTryThrowSite;
  public:
    ContextScope(CheckErrorCoverage &self)
      : Self(self), OldContext(self.CurContext),
        OldIsInTry(self.IsInTry),
        OldHasAnyThrowSite(self.HasAnyThrowSite),
        OldHasTryThrowSite(self.HasTryThrowSite) {
    }

    ContextScope(const ContextScope &) = delete;
    ContextScope &operator=(const ContextScope &) = delete;

    void makeIndependentContext() {
      Self.IsInTry = false;
      Self.HasAnyThrowSite = false;
      Self.HasTryThrowSite = false;
    }

    void preserveInfoFromAutoclosureBody() {
      // An autoclosure body is the part of the enclosing function
      // body for the purposes of deciding whether a try contained
      // a throwing call.
      OldHasTryThrowSite = Self.HasTryThrowSite;
    }

    void preserveInfoFromNonExhaustiveCatch() {
      OldHasAnyThrowSite = Self.HasAnyThrowSite;
    }

    void preserveInfoFromTryOperand() {
      OldHasAnyThrowSite = Self.HasAnyThrowSite;
    }

    ~ContextScope() {
      Self.CurContext = OldContext;
      Self.IsInTry = OldIsInTry;
      Self.HasAnyThrowSite = OldHasAnyThrowSite;
      Self.HasTryThrowSite = OldHasTryThrowSite;
    }
  };

public:
  CheckErrorCoverage(TypeChecker &tc, Context initialContext)
    : TC(tc), CurContext(initialContext) {}

  void checkClosureBody(ClosureExpr *E) {
    ContextScope scope(*this);
    CurContext = Context::forClosure(E);
    scope.makeIndependentContext();
    E->getBody()->walk(*this);
  }

  void checkAutoClosureBody(AutoClosureExpr *E) {
    ContextScope scope(*this);
    CurContext = Context::forClosure(E);
    E->getBody()->walk(*this);
    scope.preserveInfoFromAutoclosureBody();
  }

private:
  bool walkToDeclPre(Decl *D) override {
    // Skip the implementations of all local declarations... except
    // PBD.  We should really just have a PatternBindingStmt.
    return isa<PatternBindingDecl>(D);
  }

  std::pair<bool, Expr*> walkToExprPre(Expr *E) override {
    // Check explicit closures.
    if (auto closure = dyn_cast<ClosureExpr>(E)) {
      checkClosureBody(closure);
      return {false, E};
    }

    // Check autoclosures.
    //
    // TODO: when passing an autoclosure to a rethrows function,
    // we should diagnose as if the autoclosure didn't exist.
    if (auto autoclosure = dyn_cast<AutoClosureExpr>(E)) {
      checkAutoClosureBody(autoclosure);
      return {false, E};
    }

    // Handle 'try' differently.
    if (auto tryExpr = dyn_cast<TryExpr>(E)) {
      checkTry(tryExpr);
      return {false, E};
    }

    // To preserve source order of diagnostics, check the validity
    // of throwing operations before checking their sub-expressions.
    if (auto apply = dyn_cast<ApplyExpr>(E)) {
      checkApply(apply);
    } else if (auto thr = dyn_cast<ThrowExpr>(E)) {
      checkThrow(thr);
    }

    return {true, E};
  }

  std::pair<bool, Stmt*> walkToStmtPre(Stmt *S) override {
    if (auto doCatch = dyn_cast<DoCatchStmt>(S)) {
      checkDoCatch(doCatch);
      return {false, S};
    }

    assert(!isa<CatchStmt>(S));

    return {true, S};
  }

  void checkDoCatch(DoCatchStmt *S) {
    // Handle the 'do' clause.
    {
      ContextScope scope(*this);
      assert(!IsInTry && "do/catch within try?");
      scope.makeIndependentContext();

      // If the catches are exhaustive, then the 'do' clause
      // is in a handled context.
      if (S->isSyntacticallyExhaustive()) {
        CurContext = Context::getHandled();

      // Otherwise, if the enclosing context isn't handled,
      // use a specialized diagnostic about non-exhaustive catches.
      } else if (!CurContext.isHandled()) {
        CurContext = Context::forNonExhaustiveCatch(S);
      }

      S->getBody()->walk(*this);

      // Complain if nothing threw within the body.
      if (!HasAnyThrowSite) {
        TC.diagnose(S->getDoLoc(), diag::no_throw_in_catch);
      }

      if (!S->isSyntacticallyExhaustive()) {
        scope.preserveInfoFromNonExhaustiveCatch();
      }
    }

    for (auto clause : S->getCatches()) {
      checkCatch(clause);
    }
  }

  void checkCatch(CatchStmt *S) {
    // The pattern and guard aren't allowed to throw.
    {
      ContextScope scope(*this);

      CurContext = Context::forCatchPattern(S);
      S->getErrorPattern()->walk(*this);

      if (auto guard = S->getGuardExpr()) {
        CurContext = Context::forCatchGuard(S);
        guard->walk(*this);
      }
    }

    // The catch body just happens in the enclosing context.
    S->getBody()->walk(*this);
  }

  void checkApply(ApplyExpr *E) {
    // An apply expression is a potential throw site if the function throws.
    auto fnType = E->getFn()->getType()->getAs<FunctionType>();
    if (!fnType || !fnType->throws()) return;

    // TODO: filter out non-throwing calls to rethrows here.

    checkThrowSite(E, /*requiresTry*/ true);
  }

  void checkThrow(ThrowExpr *E) {
    checkThrowSite(E, /*requiresTry*/ false);
  }

  void checkThrowSite(Expr *E, bool requiresTry) {
    HasAnyThrowSite = true;
    HasTryThrowSite |= requiresTry;

    bool isErrorHandled = CurContext.isHandled();
    bool isTryCovered = (requiresTry && IsInTry);
    if (!isErrorHandled) {
      CurContext.diagnoseUnhandledThrowSite(TC, E, isTryCovered);
    } else if (!isTryCovered) {
      TC.diagnose(E->getLoc(), diag::throwing_call_without_try);
    }
  }

  void checkTry(TryExpr *E) {
    // Walk the operand.
    ContextScope scope(*this);
    IsInTry = true;
    HasTryThrowSite = false;

    E->getSubExpr()->walk(*this);

    // Diagnose 'try' expressions that weren't actually needed.
    if (!HasTryThrowSite) {
      TC.diagnose(E->getLoc(), diag::no_throw_in_try);

    // Diagnose all the call sites within a single unhandled 'try'
    // at the same time.
    } else if (!CurContext.isHandled()) {
      CurContext.diagnoseUnhandledTry(TC, E);
    }

    scope.preserveInfoFromTryOperand();
  }
};

} // end anonymous namespace 
