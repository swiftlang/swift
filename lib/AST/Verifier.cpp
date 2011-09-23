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
using namespace swift;

namespace {
  enum ShouldHalt { Continue, Halt };

  class Verifier {
    ASTContext &Context;
    VerificationKind Stage;
  public:
    Verifier(ASTContext &Context, VerificationKind Stage)
      : Context(Context), Stage(Stage) {}

    Expr *dispatch(Expr *E, WalkOrder Ord) {
      switch (E->getKind()) {
#define DISPATCH(ID) visit(static_cast<ID##Expr*>(E), Ord);
#define EXPR(ID, PARENT) \
      case ExprKind::ID: \
        return DISPATCH(ID);
#define UNCHECKED_EXPR(ID, PARENT) \
      case ExprKind::ID: \
        assert(Stage < VerificationKind::CheckedTypes && #ID "in wrong phase"); \
        return DISPATCH(ID);
#define UNBOUND_EXPR(ID, PARENT) \
      case ExprKind::ID: \
        assert(Stage < VerificationKind::BoundNames && #ID "in wrong phase"); \
        return DISPATCH(ID);
#include "swift/AST/ExprNodes.def"
#undef DISPATCH
      }
      llvm_unreachable("not all cases handled!");
    }

    Stmt *dispatch(Stmt *S, WalkOrder Ord) {
      switch (S->getKind()) {
#define DISPATCH(ID) visit(static_cast<ID##Stmt*>(S), Ord);
#define STMT(ID, PARENT) \
      case StmtKind::ID: \
        return DISPATCH(ID);
#include "swift/AST/StmtNodes.def"
#undef DISPATCH
      }
      llvm_unreachable("not all cases handled!");
    }

    Expr *visit(Expr *E, WalkOrder Ord) { return E; }
    Stmt *visit(Stmt *S, WalkOrder Ord) { return S; }
  };
}

void swift::verify(TranslationUnit *TUnit, VerificationKind Stage) {
  // Make a verifier object, and then capture it by reference.
  Verifier VObject(TUnit->Ctx, Stage);
  Verifier *V = &VObject;

  TUnit->Body->walk(^(Expr *E, WalkOrder Ord) { return V->dispatch(E, Ord); },
                    ^(Stmt *S, WalkOrder Ord) { return V->dispatch(S, Ord); });
}
