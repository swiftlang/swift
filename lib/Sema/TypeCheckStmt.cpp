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
#include "TypeChecking.h"
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
    if (typeCheckExpr(S->Dest) ||
        typeCheckExpr(S->Src, S->Dest->Ty))
      return 0;
    
    return S;
  }
  
  Stmt *visitBraceStmt(BraceStmt *BS);
  
  Stmt *visitReturnStmt(ReturnStmt *RS) {
    if (TheFunc == 0) {
      TC.error(RS->ReturnLoc, "return invalid outside of a func");
      return 0;
    }

    if (typeCheckExpr(RS->Result, TheFunc->Ty->castTo<FunctionType>()->Result))
      return 0;

    return RS;
  }
  
  Stmt *visitIfStmt(IfStmt *IS) {
    // The if condition must have __builtin_int1 type.  This is after the
    // conversion function is added by sema.
    if (typeCheckExpr(IS->Cond, TC.Context.TheInt1Type) ||
        typeCheckStmt(IS->Then) ||
        (IS->Else && typeCheckStmt(IS->Else)))
      return 0;
    
    return IS;
  }
  
  Stmt *visitWhileStmt(WhileStmt *WS) {
    // The if condition must have __builtin_int1 type.  This is after the
    // conversion function is added by sema.
    if (typeCheckExpr(WS->Cond, TC.Context.TheInt1Type) ||
        typeCheckStmt(WS->Body))
      return 0;
    
    return WS;
  }
};
  
} // end anonymous namespace
  
  
Stmt *StmtChecker::visitBraceStmt(BraceStmt *BS) {
  SmallVector<BraceStmt::ExprStmtOrDecl, 32> NewElements;
  
  for (unsigned i = 0, e = BS->NumElements; i != e; ++i) {
    if (Expr *SubExpr = BS->Elements[i].dyn_cast<Expr*>()) {
      if (typeCheckExpr(SubExpr)) continue;
        
      // If any of the elements of the braces has a function type (which
      // indicates that a function didn't get called), then produce an error.
      // TODO: What about tuples which contain functions by-value that are
      // dead?
      // TODO: QOI: Add source range.
      if (SubExpr->Ty->is<FunctionType>())
        TC.error(SubExpr->getLocStart(),
                 "expression resolves to an unevaluated function");
      
      NewElements.push_back(SubExpr);
      continue;
    }
    
    if (Stmt *SubStmt = BS->Elements[i].dyn_cast<Stmt*>()) {
      if (!typeCheckStmt(SubStmt))
        NewElements.push_back(SubStmt);
    } else {
      Decl *D = BS->Elements[i].get<Decl*>();
      TC.typeCheckDecl(D);
      NewElements.push_back(D);
    }
  }
  
  // Reinstall the list now that we potentially mutated it.
  assert(NewElements.size() <= BS->NumElements);
  memcpy(BS->Elements, NewElements.data(),
         NewElements.size()*sizeof(BS->Elements[0]));
  BS->NumElements = NewElements.size();
  return BS;
}


/// performTypeChecking - Once parsing and namebinding are complete, these
/// walks the AST to resolve types and diagnose problems therein.
///
/// FIXME: This should be moved out to somewhere else.
void swift::performTypeChecking(TranslationUnit *TU, ASTContext &Ctx) {
  TypeChecker TC(Ctx);
  
  // Find all the FuncExprs in the translation unit.
  std::vector<FuncExpr*> FuncExprs;
  auto FuncExprsP = &FuncExprs;            // Blocks are annoying.
  TU->Body->walk(^(Expr *E, WalkOrder Order) {
    if (Order == WalkOrder::PreOrder)
      if (FuncExpr *FE = dyn_cast<FuncExpr>(E))
        FuncExprsP->push_back(FE);
    return E;
  });

  // Type check the top-level BraceExpr.  This sorts out any top-level
  // expressions and variable decls.
  StmtChecker(TC, 0).typeCheckStmt(TU->Body);

  // Type check the body of each of the FuncExpr in turn.
  for (FuncExpr *FE : FuncExprs) {
    if (!FE->Body) continue;
    
    StmtChecker(TC, FE).typeCheckStmt(FE->Body);
  }
}
