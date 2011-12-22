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
 
  bool typeCheckConversion(Expr *&E, Type T) {
    Expr *E2 = TC.convertToType(E, T);
    if (E2 == 0) return true;
    E = E2;
    return false;
  }
  
  
  //===--------------------------------------------------------------------===//
  // Visit Methods.
  //===--------------------------------------------------------------------===//

  Stmt *visitSemiStmt(SemiStmt *S) {
    return S;
  }

  Stmt *visitAssignStmt(AssignStmt *S) {
    Expr *E = S->getDest();
    if (typeCheckExpr(E)) return 0;
    S->setDest(E);

    E = S->getSrc();
    if (typeCheckExpr(E, S->getDest()->getType())) return 0;
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
    if (typeCheckExpr(E, TheFunc->getType()->castTo<FunctionType>()->Result))
      return 0;
    RS->setResult(E);

    return RS;
  }
  
  Stmt *visitIfStmt(IfStmt *IS) {
    // The if condition must have Builtin::int1 type.  This is after the
    // conversion function is added by sema.
    Expr *E = IS->getCond();
    if (typeCheckExpr(E, BuiltinIntegerType::get(1, TC.Context))) return 0;
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
    // The if condition must have Builtin::int1 type.  This is after the
    // conversion function is added by sema.
    Expr *E = WS->getCond();
    if (typeCheckExpr(E, BuiltinIntegerType::get(1, TC.Context))) return 0;
    WS->setCond(E);

    Stmt *S = WS->getBody();
    if (typeCheckStmt(S)) return 0;
    WS->setBody(S);
    
    return WS;
  }
};
  
} // end anonymous namespace
  
  
Stmt *StmtChecker::visitBraceStmt(BraceStmt *BS) {
  SmallVector<BraceStmt::ExprStmtOrDecl, 32> NewElements;
  
  for (unsigned i = 0, e = BS->getNumElements(); i != e; ++i) {
    if (Expr *SubExpr = BS->getElement(i).dyn_cast<Expr*>()) {
      if (typeCheckExpr(SubExpr)) continue;
        
      // If any of the elements of the braces has a function type (which
      // indicates that a function didn't get called), then produce an error.
      // TODO: What about tuples which contain functions by-value that are
      // dead?
      // FIXME: TODO: QOI: Add source range.
      if (SubExpr->getType()->is<FunctionType>())
        TC.diagnose(SubExpr->getLoc(), diag::expression_unresolved_function);
      
      BS->setElement(i, SubExpr);
      continue;
    }
    
    if (Stmt *SubStmt = BS->getElement(i).dyn_cast<Stmt*>()) {
      if (!typeCheckStmt(SubStmt))
        BS->setElement(i, SubStmt);
    } else {
      Decl *D = BS->getElement(i).get<Decl*>();
      TC.typeCheckDecl(D);
      NewElements.push_back(D);
    }
  }
  
  return BS;
}

/// performTypeChecking - Once parsing and namebinding are complete, these
/// walks the AST to resolve types and diagnose problems therein.
///
/// FIXME: This should be moved out to somewhere else.
void swift::performTypeChecking(TranslationUnit *TU) {
  TypeChecker TC(*TU);
  
  // Find all the FuncExprs in the translation unit and collapse all
  // the sequences.
  std::vector<FuncExpr*> FuncExprs;
  auto FuncExprs_ptr = &FuncExprs;            // Blocks are annoying.
  auto TC_ptr = &TC;                          // Again.
  TU->Body->walk(^(Expr *E, WalkOrder Order, WalkContext const&) {
    if (Order == WalkOrder::PreOrder) {
      if (FuncExpr *FE = dyn_cast<FuncExpr>(E))
        FuncExprs_ptr->push_back(FE);
    } else {
      if (SequenceExpr *SE = dyn_cast<SequenceExpr>(E))
        return TC_ptr->foldSequence(SE);
    }
    return E;
  });

  // Type check the top-level BraceExpr.  This sorts out any top-level
  // expressions and variable decls.
  StmtChecker(TC, 0).typeCheckStmt(TU->Body);

  // Type check the body of each of the FuncExpr in turn.
  for (FuncExpr *FE : FuncExprs) {
    if (!FE->getBody()) continue;
    
    BraceStmt *S = FE->getBody();
    StmtChecker(TC, FE).typeCheckStmt(S);
    FE->setBody(S);
  }

  // Verify that we've checked types correctly.
  TU->ASTStage = TranslationUnit::TypeChecked;
  verify(TU);
}
