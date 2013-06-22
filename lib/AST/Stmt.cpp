//===--- Stmt.cpp - Swift Language Statement ASTs -------------------------===//
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
//  This file implements the Stmt class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Stmt.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "llvm/ADT/PointerUnion.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// Stmt methods.
//===----------------------------------------------------------------------===//

// Only allow allocation of Stmts using the allocator in ASTContext.
void *Stmt::operator new(size_t Bytes, ASTContext &C,
                         unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

// Helper functions to verify statically whether the getSourceRange()
// function has been overridden.
typedef const char (&TwoChars)[2];

template<typename Class> 
inline char checkSourceRangeType(SourceRange (Class::*)() const);

inline TwoChars checkSourceRangeType(SourceRange (Stmt::*)() const);

SourceRange Stmt::getSourceRange() const {
  switch (Kind) {
#define STMT(ID, PARENT) \
case StmtKind::ID: \
static_assert(sizeof(checkSourceRangeType(&ID##Stmt::getSourceRange)) == 1, \
              #ID "Stmt is missing getSourceRange()"); \
return cast<ID##Stmt>(this)->getSourceRange();
#include "swift/AST/StmtNodes.def"
  }
  
  llvm_unreachable("statement type not handled!");
}

bool Stmt::isImplicit() const {
  if (auto brace = dyn_cast<BraceStmt>(this)) {
    for (auto element : brace->getElements()) {
      if (auto expr = element.dyn_cast<Expr *>()) {
        if (!expr->isImplicit())
          return false;
        continue;
      }

      if (auto stmt = element.dyn_cast<Stmt *>()) {
        if (!stmt->isImplicit())
          return false;
        continue;
      }

      if (!element.get<Decl *>()->isImplicit())
        return false;
    }
    if (brace->getLBraceLoc().isInvalid() &&
        brace->getRBraceLoc().isInvalid())
      return true;
  }

  return false;
}

BraceStmt::BraceStmt(SourceLoc lbloc, ArrayRef<ExprStmtOrDecl> elts,
                     SourceLoc rbloc)
  : Stmt(StmtKind::Brace), NumElements(elts.size()), LBLoc(lbloc), RBLoc(rbloc){
  memcpy(getElementsStorage(), elts.data(),
         elts.size() * sizeof(ExprStmtOrDecl));
}

BraceStmt *BraceStmt::create(ASTContext &ctx, SourceLoc lbloc,
                             ArrayRef<ExprStmtOrDecl> elts, SourceLoc rbloc) {
  void *Buffer = ctx.Allocate(sizeof(BraceStmt)
                                + elts.size() * sizeof(ExprStmtOrDecl),
                              alignof(BraceStmt));
  return ::new(Buffer) BraceStmt(lbloc, elts, rbloc);
}

SourceRange ReturnStmt::getSourceRange() const {
  SourceLoc Start = ReturnLoc;
  SourceLoc End = ReturnLoc;
  if (Result)
    End = Result->getEndLoc();
  if (Start.isInvalid() && Result)
    Start = Result->getStartLoc();
  
  return SourceRange(Start, End);
}

SourceRange IfStmt::getSourceRange() const {
  SourceLoc End;
  if (Else)
    End = Else->getEndLoc();
  else
    End = Then->getEndLoc();
  return SourceRange(IfLoc, End);
}

SourceRange WhileStmt::getSourceRange() const {
  return SourceRange(WhileLoc, Body->getEndLoc());
}

SourceRange DoWhileStmt::getSourceRange() const {
  return SourceRange(DoLoc, Cond->getEndLoc());
}

Pattern *ForEachStmt::getPattern() const {
  if (Pattern *P = Pat.dyn_cast<Pattern *>())
    return P;
  return Pat.get<PatternBindingDecl *>()->getPattern();
}

CaseStmt *CaseStmt::create(SourceLoc CaseOrDefaultLoc,
                           ArrayRef<Expr*> ValueExprs,
                           SourceLoc ColonLoc,
                           BraceStmt *Body,
                           ASTContext &C) {
  void *p = C.Allocate(sizeof(CaseStmt) + ValueExprs.size() * sizeof(Expr*),
                       alignof(CaseStmt));
  CaseStmt *theCase = ::new (p) CaseStmt(CaseOrDefaultLoc,
                                         ValueExprs.size(),
                                         ColonLoc, Body);
  memcpy(theCase->getValueExprBuffer(),
         ValueExprs.data(), ValueExprs.size() * sizeof(Expr*));
  return theCase;
}

SwitchStmt *SwitchStmt::create(SourceLoc SwitchLoc,
                               Expr *SubjectExpr,
                               VarDecl *SubjectDecl,
                               SourceLoc LBraceLoc,
                               ArrayRef<CaseStmt *> Cases,
                               SourceLoc RBraceLoc,
                               ASTContext &C) {
  void *p = C.Allocate(sizeof(SwitchStmt) + Cases.size() * sizeof(SwitchStmt*),
                       alignof(SwitchStmt));
  SwitchStmt *theSwitch = ::new (p) SwitchStmt(SwitchLoc,
                                               SubjectExpr,
                                               SubjectDecl,
                                               LBraceLoc,
                                               Cases.size(),
                                               RBraceLoc);
  memcpy(theSwitch->getCaseBuffer(),
         Cases.data(), Cases.size() * sizeof(CaseStmt*));
  return theSwitch;
}
