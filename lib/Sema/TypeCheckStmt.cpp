//===--- TypeCheckStmt.cpp - Type Checking for Statements -----------------===//
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
// This file implements semantic analysis for statements.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "TypeChecker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ASTWalker.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

namespace {
/// StmtChecker - This class implements 
class StmtChecker : public StmtVisitor<StmtChecker, Stmt*> {
public:
  TypeChecker &TC;
  
  // TheFunc - This is the current FuncExpr being checked.  This is null for
  // top level code.
  FuncExpr *TheFunc;
  
  StmtChecker(TypeChecker &TC, FuncExpr *TheFunc) : TC(TC), TheFunc(TheFunc) {
  }

  //===--------------------------------------------------------------------===//
  // Helper Functions.
  //===--------------------------------------------------------------------===//
  
  bool typeCheckExpr(Expr *&E, Type DestTy = Type()) {
    return TC.typeCheckExpression(E, DestTy);
  }

  template<typename StmtTy>
  bool typeCheckStmt(StmtTy *&S) {
    StmtTy *S2 = cast_or_null<StmtTy>(visit(S));
    if (S2 == 0) return true;
    S = S2;
    return false;
  }
 
  //===--------------------------------------------------------------------===//
  // Visit Methods.
  //===--------------------------------------------------------------------===//

  Stmt *visitErrorStmt(ErrorStmt *S) {
    return S;
  }

  Stmt *visitSemiStmt(SemiStmt *S) {
    return S;
  }

  Stmt *visitAssignStmt(AssignStmt *S) {
    Expr *E = S->getDest();
    if (typeCheckExpr(E)) return 0;
    S->setDest(E);

    Type lhsTy = E->getType();
    if (LValueType *lvalueTy = lhsTy->getAs<LValueType>())
      lhsTy = lvalueTy->getObjectType();
    else
      TC.diagnose(E->getLoc(), diag::assignment_lhs_not_lvalue);

    E = S->getSrc();
    if (typeCheckExpr(E, lhsTy)) return 0;
    S->setSrc(E);

    return S;
  }
  
  Stmt *visitBraceStmt(BraceStmt *BS);
  
  Stmt *visitReturnStmt(ReturnStmt *RS) {
    if (TheFunc == 0) {
      TC.diagnose(RS->getReturnLoc(), diag::return_invalid_outside_func);
      return 0;
    }

    Expr *E = RS->getResult();
    if (typeCheckExpr(E, TheFunc->getBodyResultType()))
      return 0;
    RS->setResult(E);

    return RS;
  }
  
  Stmt *visitIfStmt(IfStmt *IS) {
    Expr *E = IS->getCond();
    if (TC.typeCheckCondition(E)) return 0;
    IS->setCond(E);

    Stmt *S = IS->getThenStmt();
    if (typeCheckStmt(S)) return 0;
    IS->setThenStmt(S);

    if ((S = IS->getElseStmt())) {
      if (typeCheckStmt(S)) return 0;
      IS->setElseStmt(S);
    }
    
    return IS;
  }
  
  Stmt *visitWhileStmt(WhileStmt *WS) {
    Expr *E = WS->getCond();
    if (TC.typeCheckCondition(E)) return 0;
    WS->setCond(E);

    Stmt *S = WS->getBody();
    if (typeCheckStmt(S)) return 0;
    WS->setBody(S);
    
    return WS;
  }
};
  
} // end anonymous namespace
  
  
Stmt *StmtChecker::visitBraceStmt(BraceStmt *BS) {
  for (unsigned i = 0, e = BS->getNumElements(); i != e; ++i) {
    if (Expr *SubExpr = BS->getElement(i).dyn_cast<Expr*>()) {
      if (typeCheckExpr(SubExpr)) continue;
      TC.typeCheckIgnoredExpr(SubExpr);
      BS->setElement(i, SubExpr);
      continue;
    }
    
    if (Stmt *SubStmt = BS->getElement(i).dyn_cast<Stmt*>()) {
      if (!typeCheckStmt(SubStmt))
        BS->setElement(i, SubStmt);
    } else {
      Decl *D = BS->getElement(i).get<Decl*>();
      TC.typeCheckDecl(D);
    }
  }
  
  return BS;
}

/// Check an expression whose result is not being used at all.
void TypeChecker::typeCheckIgnoredExpr(Expr *E) {
  // Complain about l-values that are neither loaded nor stored.
  if (E->getType()->is<LValueType>()) {
    diagnose(E->getLoc(), diag::expression_unused_lvalue)
      << E->getSourceRange();
    return;
  }

  // Complain about functions that aren't called.
  // TODO: What about tuples which contain functions by-value that are
  // dead?
  if (E->getType()->is<FunctionType>()) {
    diagnose(E->getLoc(), diag::expression_unused_function)
      << E->getSourceRange();
    return;
  }
}

/// performTypeChecking - Once parsing and namebinding are complete, these
/// walks the AST to resolve types and diagnose problems therein.
///
/// FIXME: This should be moved out to somewhere else.
void swift::performTypeChecking(TranslationUnit *TU, unsigned StartElem) {
  TypeChecker TC(*TU);
  
  // Find all the FuncExprs in the translation unit and collapse all
  // the sequences.
  struct PrePassWalker : ASTWalker {
    TypeChecker &TC;
    SmallVector<FuncExpr*, 32> FuncExprs;

    PrePassWalker(TypeChecker &TC) : TC(TC) {}

    bool walkToExprPre(Expr *E) {
      if (FuncExpr *FE = dyn_cast<FuncExpr>(E))
        FuncExprs.push_back(FE);
      return true;
    }

    Expr *walkToExprPost(Expr *E) {
      if (SequenceExpr *SE = dyn_cast<SequenceExpr>(E))
        return TC.foldSequence(SE);
      return E;
    }
  };
  PrePassWalker prePass(TC);
  for (unsigned i = StartElem, e = TU->Body->getNumElements(); i != e; ++i) {
    auto Elem = TU->Body->getElement(i);
    if (Expr *E = Elem.dyn_cast<Expr*>())
      TU->Body->setElement(i, E->walk(prePass));
    else if (Stmt *S = Elem.dyn_cast<Stmt*>())
      TU->Body->setElement(i, S->walk(prePass));
    else
      Elem.get<Decl*>()->walk(prePass);
  }

  // Type check the top-level elements of the translation unit.
  StmtChecker checker(TC, 0);
  for (unsigned i = StartElem, e = TU->Body->getNumElements(); i != e; ++i) {
    auto Elem = TU->Body->getElement(i);
    if (Expr *E = Elem.dyn_cast<Expr*>()) {
      if (checker.typeCheckExpr(E)) continue;
      TC.typeCheckIgnoredExpr(E);
      TU->Body->setElement(i, E);
    } else if (Stmt *S = Elem.dyn_cast<Stmt*>()) {
      if (checker.typeCheckStmt(S)) continue;
      TU->Body->setElement(i, S);
    } else {
      TC.typeCheckDecl(Elem.get<Decl*>());
    }
  }

  // Type check the body of each of the FuncExpr in turn.  Note that outside
  // FuncExprs must be visited before nested FuncExprs for type-checking to
  // work correctly.
  for (FuncExpr *FE : prePass.FuncExprs) {
    TC.semaFunctionSignature(FE);

    PrettyStackTraceExpr StackEntry(TC.Context, "type-checking", FE);

    BraceStmt *S = FE->getBody();
    StmtChecker(TC, FE).typeCheckStmt(S);
    FE->setBody(S);
  }

  // Verify that we've checked types correctly.
  TU->ASTStage = TranslationUnit::TypeChecked;
  verify(TU);
}
