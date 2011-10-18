//===--- Verifier.cpp - AST Invariant Verification ------------------------===//
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
//  This file implements a verifier of AST invariants.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Verifier.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

namespace {
  enum ShouldHalt { Continue, Halt };

  class Verifier {
    VerificationKind Stage;
    llvm::raw_ostream &Out;

  public:
    Verifier(VerificationKind Stage)
      : Stage(Stage), Out(llvm::errs()) {}

    Expr *dispatch(Expr *E, WalkOrder ord) {
      switch (E->getKind()) {
#define DISPATCH(ID) return dispatchVisit(static_cast<ID##Expr*>(E), ord)
#define EXPR(ID, PARENT) \
      case ExprKind::ID: \
        DISPATCH(ID);
#define UNCHECKED_EXPR(ID, PARENT) \
      case ExprKind::ID: \
        assert(Stage < VerificationKind::CheckedTypes && #ID "in wrong phase"); \
        DISPATCH(ID);
#define UNBOUND_EXPR(ID, PARENT) \
      case ExprKind::ID: \
        assert(Stage < VerificationKind::BoundNames && #ID "in wrong phase"); \
        DISPATCH(ID);
#include "swift/AST/ExprNodes.def"
#undef DISPATCH
      }
      llvm_unreachable("not all cases handled!");
    }

    Stmt *dispatch(Stmt *S, WalkOrder ord) {
      switch (S->getKind()) {
#define DISPATCH(ID) return dispatchVisit(static_cast<ID##Stmt*>(S), ord)
#define STMT(ID, PARENT) \
      case StmtKind::ID: \
        DISPATCH(ID);
#include "swift/AST/StmtNodes.def"
#undef DISPATCH
      }
      llvm_unreachable("not all cases handled!");
    }

  private:
    /// Helper template for dispatching visitation.
    template <class T> T dispatchVisit(T node, WalkOrder ord) {
      // If we're visiting in pre-order, don't validate the node yet;
      // just check whether we should stop further descent.
      if (ord == WalkOrder::PreOrder)
        return (shouldVerify(node) ? node : T());

      // Otherwise, actually verify the node.

      // Always verify the node as a parsed node.
      verifyParsed(node);

      // If we've bound names already, verify as a bound node.
      if (Stage >= VerificationKind::BoundNames)
        verifyBound(node);

      // If we've checked types already, do some extra verification.
      if (Stage >= VerificationKind::CheckedTypes)
        verifyChecked(node);

      // Always continue.
      return node;
    }

    // Default cases for whether we should verify within the given subtree.
    bool shouldVerify(Expr *E) { return true; }
    bool shouldVerify(Stmt *S) { return true; }

    // Base cases for the various stages of verification.
    void verifyParsed(Expr *E) {}
    void verifyParsed(Stmt *S) {}
    void verifyBound(Expr *E) {}
    void verifyBound(Stmt *S) {}
    void verifyChecked(Expr *E) {}
    void verifyChecked(Stmt *S) {}

    // Specialized verifiers.

    void verifyChecked(AssignStmt *S) {
      checkSameType(S->getDest()->getType(), S->getSrc()->getType(),
                    "assignment operands");
    }

    // Verification utilities.
    void checkSameType(Type T0, Type T1, const char *what) {
      if (T0->getCanonicalType() == T1->getCanonicalType())
        return;

      Out << "different types for " << what << ": ";
      T0.print(Out);
      Out << " vs. ";
      T1.print(Out);
      Out << "\n";
      abort();
    }
  };
}

void swift::verify(TranslationUnit *TUnit, VerificationKind Stage) {
  // For now, punt if there are errors in the translation unit.
  if (TUnit->Ctx.hadError()) return;

  // Make a verifier object, and then capture it by reference.
  Verifier VObject(Stage);
  Verifier *V = &VObject;

  TUnit->Body->walk(^(Expr *E, WalkOrder Ord) { return V->dispatch(E, Ord); },
                    ^(Stmt *S, WalkOrder Ord) { return V->dispatch(S, Ord); });
}
