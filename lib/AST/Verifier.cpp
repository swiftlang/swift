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

    Expr *dispatch(Expr *E, WalkOrder ord, WalkContext const& WalkCtx) {
      switch (E->getKind()) {
#define DISPATCH(ID) return dispatchVisit(static_cast<ID##Expr*>(E), ord, \
                                          WalkCtx)
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

    Stmt *dispatch(Stmt *S, WalkOrder ord, WalkContext const& WalkCtx) {
      switch (S->getKind()) {
#define DISPATCH(ID) return dispatchVisit(static_cast<ID##Stmt*>(S), ord, \
                                          WalkCtx)
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
    template <class T> T dispatchVisit(T node, WalkOrder ord, 
                                       WalkContext const& WalkCtx) {
      // If we're visiting in pre-order, don't validate the node yet;
      // just check whether we should stop further descent.
      if (ord == WalkOrder::PreOrder)
        return (shouldVerify(node) ? node : T());

      // Otherwise, actually verify the node.

      // Always verify the node as a parsed node.
      verifyParsed(node, WalkCtx);

      // If we've bound names already, verify as a bound node.
      if (Stage >= VerificationKind::BoundNames)
        verifyBound(node, WalkCtx);

      // If we've checked types already, do some extra verification.
      if (Stage >= VerificationKind::CheckedTypes)
        verifyChecked(node, WalkCtx);

      // Always continue.
      return node;
    }

    // Default cases for whether we should verify within the given subtree.
    bool shouldVerify(Expr *E) { return true; }
    bool shouldVerify(Stmt *S) { return true; }

    // Base cases for the various stages of verification.
    void verifyParsed(Expr *E, WalkContext const& WalkCtx) {
      checkSourceRanges(E, WalkCtx);
    }
    void verifyParsed(Stmt *S, WalkContext const& WalkCtx) {
      checkSourceRanges(S, WalkCtx);
    }
    void verifyBound(Expr *E, WalkContext const& WalkCtx) {
      checkSourceRanges(E, WalkCtx);
    }
    void verifyBound(Stmt *S, WalkContext const& WalkCtx) {
      checkSourceRanges(S, WalkCtx);
    }
    void verifyChecked(Expr *E, WalkContext const& WalkCtx) {
      checkSourceRanges(E, WalkCtx);
    }
    void verifyChecked(Stmt *S, WalkContext const& WalkCtx) {
      checkSourceRanges(S, WalkCtx);
    }

    // Specialized verifiers.

    void verifyChecked(AssignStmt *S, WalkContext const& WalkCtx) {
      verifyChecked(static_cast<Stmt *>(S), WalkCtx);
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
    
    void checkSourceRanges(Expr *E, WalkContext const& WalkCtx) {
      if (!E->getSourceRange().isValid()) {
        Out << "invalid source range for expression: ";
        E->print(Out);
        Out << "\n";
        abort();
      }
      checkSourceRanges(E->getSourceRange(), WalkCtx.Parent,
                        ^ { E->print(Out); } );
    }
    
    void checkSourceRanges(Stmt *S, WalkContext const &WalkCtx) {
      if (!S->getSourceRange().isValid()) {
        Out << "invalid source range for statement: ";
        S->print(Out);
        Out << "\n";
        abort();
      }
      checkSourceRanges(S->getSourceRange(), WalkCtx.Parent,
                        ^ { S->print(Out); });
    }
    
    /// \brief Verify that the given source ranges is contained within the
    /// parent's source range.
    void checkSourceRanges(SourceRange Current, 
                           llvm::PointerUnion<Expr *, Stmt *> Parent,
                           void (^printEntity)()) {
      SourceRange Enclosing;
      if (Stmt *S = Parent.dyn_cast<Stmt *>())
        Enclosing = S->getSourceRange();
      else if (Expr *E = Parent.dyn_cast<Expr *>())
        Enclosing = E->getSourceRange();
      else // no parent
        return;
      
      // FIXME: This is a very ugly way to check inclusion.
      if (Enclosing.Start.Value.getPointer() > Current.Start.Value.getPointer()
          || Enclosing.End.Value.getPointer() < Current.End.Value.getPointer()) 
      {
        Out << "child source range not contained within its parent: ";
        printEntity();
        Out << "\n";
        abort();
      }
    }
    
  };
}

void swift::verify(TranslationUnit *TUnit, VerificationKind Stage) {
  // FIXME: For now, punt if there are errors in the translation unit.
  if (TUnit->Ctx.hadError()) return;

  // Make a verifier object, and then capture it by reference.
  Verifier VObject(Stage);
  Verifier *V = &VObject;

  TUnit->Body->walk(^(Expr *E, WalkOrder Ord, WalkContext const &WalkCtx) { 
                        return V->dispatch(E, Ord, WalkCtx); 
                    },
                    ^(Stmt *S, WalkOrder Ord, WalkContext const &WalkCtx) { 
                        return V->dispatch(S, Ord, WalkCtx); 
                    });
}
