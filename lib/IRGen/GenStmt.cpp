//===--- GenStmt.cpp - IR Generation for Statements -----------------------===//
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
//  This file implements IR generation for Swift statements.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/ErrorHandling.h"
#include "Cleanup.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "JumpDest.h"
#include "LValue.h"
#include "RValue.h"

using namespace swift;
using namespace irgen;

void IRGenFunction::emitStmt(Stmt *S) {
  switch (S->Kind) {
  case StmtKind::Semi:
    // Nothing to do.
    return;

  case StmtKind::Assign:
    return emitAssignStmt(cast<AssignStmt>(S));

  case StmtKind::Brace:
    return emitBraceStmt(cast<BraceStmt>(S));

  case StmtKind::Return:
    return emitReturnStmt(cast<ReturnStmt>(S));

  case StmtKind::If:
    return emitIfStmt(cast<IfStmt>(S));

  case StmtKind::While:
    return emitWhileStmt(cast<WhileStmt>(S));
  }
  llvm_unreachable("bad statement kind!");
}

void IRGenFunction::emitBraceStmt(BraceStmt *BS) {
  // Enter a new scope.
  Scope BraceScope(*this);

  for (unsigned I = 0, E = BS->NumElements; I != E; ++I) {
    BraceStmt::ExprStmtOrDecl Elt = BS->Elements[I];
    if (Expr *E = Elt.dyn_cast<Expr*>()) {
      emitIgnored(E);
    } else if (Stmt *S = Elt.dyn_cast<Stmt*>()) {
      emitStmt(S);
    } else {
      emitLocal(Elt.get<Decl*>());
    }
  }
}

void IRGenFunction::emitAssignStmt(AssignStmt *S) {
  // Emit the LHS.
  const TypeInfo &TInfo = getFragileTypeInfo(S->Dest->Ty);
  LValue LV = emitLValue(S->Dest, TInfo);

  // Emit the RHS.
  RValue RV = emitRValue(S->Src);

  // Do the store.
  TInfo.store(*this, RV, LV);
}

void IRGenFunction::emitIfStmt(IfStmt *S) {
  
  unimplemented(S->getLocStart(), "IfStmt is unimplemented");
}

void IRGenFunction::emitReturnStmt(ReturnStmt *S) {
  // The expression is evaluated in a full-expression context.
  FullExpr FullExpr(*this);

  // If this function takes no return value, ignore the result of the
  // expression.
  if (!ReturnSlot.isValid()) {
    emitIgnored(S->Result);
  } else {
    const TypeInfo &ResultInfo = getFragileTypeInfo(S->Result->Ty);
    RValue RV = emitRValue(S->Result, ResultInfo);
    ResultInfo.store(*this, RV, ReturnSlot);
  }

  // Leave the full-expression.
  FullExpr.pop();

  // In either case, branch to the return block.
  JumpDest ReturnDest(ReturnBB);
  emitBranch(ReturnDest);
}

void IRGenFunction::emitWhileStmt(WhileStmt *S) {
  unimplemented(S->getLocStart(), "WhileStmt is unimplemented");
}
