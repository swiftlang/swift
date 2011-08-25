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
  
  bool typeCheckExpr(Expr *&E) {
    Expr *E2 = TC.typeCheckExpression(E);
    if (E2 == 0) return true;
    E = E2;
    return false;
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

  Stmt *visitSemiStmt(SemiStmt *S) {
    return S;
  }

  Stmt *visitAssignStmt(AssignStmt *S) {
    if (typeCheckExpr(S->Src) || typeCheckExpr(S->Dest))
      return 0;
    
    // Coerce the source to the destination type.
    S->Src = TC.convertToType(S->Src, S->Dest->Ty);
    if (S->Src == 0) {
      TC.note(S->EqualLoc,
              "while converting assigned value to destination type");
      return 0;
    }
    
    return S;
  }
  
  Stmt *visitBraceStmt(BraceStmt *BS);
  
  Stmt *visitReturnStmt(ReturnStmt *RS) {
    if (typeCheckExpr(RS->Result))
      return 0;
    
    // If TheFunc == 0 error.
    
    // FIXME: Convert the subexpr to the return type of TheFunc.
    return RS;
  }
  
  Stmt *visitIfStmt(IfStmt *IS) {
    if (typeCheckExpr(IS->Cond) || typeCheckStmt(IS->Then) ||
        (IS->Else && typeCheckStmt(IS->Else)))
      return 0;
    
    // The if condition must have __builtin_int1 type.  This is after the
    // conversion function is added by sema.
    IS->Cond = TC.convertToType(IS->Cond, TC.Context.TheInt1Type);
    if (IS->Cond == 0)
      return 0;
    
    return IS;
  }
  
  Stmt *visitWhileStmt(WhileStmt *WS) {
    if (typeCheckExpr(WS->Cond) || typeCheckStmt(WS->Body))
      return 0;
    
    // The if condition must have __builtin_int1 type.  This is after the
    // conversion function is added by sema.
    WS->Cond = TC.convertToType(WS->Cond, TC.Context.TheInt1Type);
    if (WS->Cond == 0)
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
      continue;
    }

    // FIXME: Temporary for compatibility.
    Decl *D = BS->Elements[i].get<Decl*>();
    NewElements.push_back(D);
    
    if (TypeAliasDecl *TAD = dyn_cast<TypeAliasDecl>(D))
      TC.typeCheck(TAD);
    
    if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
      TC.typeCheck(VD);
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
void swift::performTypeChecking(TranslationUnitDecl *TUD, ASTContext &Ctx) {
  TypeChecker TC(Ctx);
  
  // Find all the FuncExprs in the translation unit.
  std::vector<FuncExpr*> FuncExprs;
  auto FuncExprsP = &FuncExprs;            // Blocks are annoying.
  TUD->Body->walk(^(Expr *E, WalkOrder Order) {
    if (Order == WalkOrder::PreOrder)
      if (FuncExpr *FE = dyn_cast<FuncExpr>(E))
        FuncExprsP->push_back(FE);
    return E;
  });
  
  // Type check the body of each of the FuncExpr in turn.
  for (FuncExpr *FE : FuncExprs) {
    if (!FE->Body) continue;
    
    StmtChecker(TC, FE).typeCheckStmt(FE->Body);
  }
  
  // Type check the top-level BraceExpr.  This sorts out any top-level
  // expressions and recursively processes the rest of the translation unit.
  StmtChecker(TC, 0).typeCheckStmt(TUD->Body);
}
