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

StringRef Stmt::getKindName(StmtKind K) {
  switch (K) {
#define STMT(Id, Parent) case StmtKind::Id: return #Id;
#include "swift/AST/StmtNodes.def"
  }
}

// Helper functions to verify statically whether the getSourceRange()
// function has been overridden.
typedef const char (&TwoChars)[2];

template<typename Class> 
inline char checkSourceRangeType(SourceRange (Class::*)() const);

inline TwoChars checkSourceRangeType(SourceRange (Stmt::*)() const);

SourceRange Stmt::getSourceRange() const {
  switch (getKind()) {
#define STMT(ID, PARENT) \
case StmtKind::ID: \
static_assert(sizeof(checkSourceRangeType(&ID##Stmt::getSourceRange)) == 1, \
              #ID "Stmt is missing getSourceRange()"); \
return cast<ID##Stmt>(this)->getSourceRange();
#include "swift/AST/StmtNodes.def"
  }
  
  llvm_unreachable("statement type not handled!");
}

BraceStmt::BraceStmt(SourceLoc lbloc, ArrayRef<ExprStmtOrDecl> elts,
                     SourceLoc rbloc, Optional<bool> implicit)
  : Stmt(StmtKind::Brace, getDefaultImplicitFlag(implicit, lbloc)),
    NumElements(elts.size()), LBLoc(lbloc), RBLoc(rbloc)
{
  memcpy(getElementsStorage(), elts.data(),
         elts.size() * sizeof(ExprStmtOrDecl));
}

BraceStmt *BraceStmt::create(ASTContext &ctx, SourceLoc lbloc,
                             ArrayRef<ExprStmtOrDecl> elts, SourceLoc rbloc,
                             Optional<bool> implicit) {
  void *Buffer = ctx.Allocate(sizeof(BraceStmt)
                                + elts.size() * sizeof(ExprStmtOrDecl),
                              alignof(BraceStmt));
  return ::new(Buffer) BraceStmt(lbloc, elts, rbloc, implicit);
}

SourceLoc BraceStmt::getElementStartLoc(const ExprStmtOrDecl &ASTNode) {
  if (const Expr *E = ASTNode.dyn_cast<Expr*>())
    return E->getStartLoc();
  if (const Stmt *S = ASTNode.dyn_cast<Stmt*>())
    return S->getStartLoc();
  if (const Decl *D = ASTNode.dyn_cast<Decl*>())
    return D->getStartLoc();
  llvm_unreachable("unsupported AST node");
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

CaseLabel::CaseLabel(bool isDefault,
                     SourceLoc caseLoc, ArrayRef<Pattern*> patterns,
                     SourceLoc whereLoc, Expr *guardExpr,
                     SourceLoc colonLoc)
  : CaseLoc(caseLoc), ColonLoc(colonLoc), WhereLoc(whereLoc),
    GuardExprAndIsDefault(guardExpr, isDefault),
    NumPatterns(patterns.size())
{
  MutableArrayRef<Pattern*> patternBuf{getPatternsBuffer(), NumPatterns};
  
  for (unsigned i = 0; i < NumPatterns; ++i) {
    patternBuf[i] = patterns[i];
  }
}

CaseLabel *CaseLabel::create(ASTContext &C, bool isDefault,
                             SourceLoc caseLoc, ArrayRef<Pattern *> patterns,
                             SourceLoc whereLoc, Expr *guardExpr,
                             SourceLoc colonLoc) {
  void *buf = C.Allocate(sizeof(CaseLabel) + sizeof(Pattern*) * patterns.size(),
                         alignof(CaseLabel));
  return ::new (buf) CaseLabel(isDefault,
                               caseLoc, patterns,
                               whereLoc, guardExpr, colonLoc);
}

CaseStmt::CaseStmt(ArrayRef<CaseLabel*> Labels, bool HasBoundDecls, Stmt *Body,
                   Optional<bool> implicit)
  : Stmt(StmtKind::Case,
         getDefaultImplicitFlag(implicit, Labels[0]->getCaseLoc())),
    BodyAndHasBoundDecls(Body, HasBoundDecls),
    NumCaseLabels(Labels.size())
{
  assert(NumCaseLabels > 0 && "case block must have at least one label");
  MutableArrayRef<CaseLabel*> buf{getCaseLabelsBuffer(), NumCaseLabels};
  
  for (unsigned i = 0; i < NumCaseLabels; ++i) {
    buf[i] = Labels[i];
  }
}

CaseStmt *CaseStmt::create(ASTContext &C,
                           ArrayRef<CaseLabel*> Labels,
                           bool HasBoundDecls,
                           Stmt *Body,
                           Optional<bool> implicit) {
  void *p = C.Allocate(sizeof(CaseStmt) + Labels.size() * sizeof(CaseLabel*),
                       alignof(CaseStmt));
  return ::new (p) CaseStmt(Labels, HasBoundDecls, Body, implicit);
}

SwitchStmt *SwitchStmt::create(SourceLoc SwitchLoc,
                               Expr *SubjectExpr,
                               SourceLoc LBraceLoc,
                               ArrayRef<CaseStmt *> Cases,
                               SourceLoc RBraceLoc,
                               ASTContext &C) {
  void *p = C.Allocate(sizeof(SwitchStmt) + Cases.size() * sizeof(SwitchStmt*),
                       alignof(SwitchStmt));
  SwitchStmt *theSwitch = ::new (p) SwitchStmt(SwitchLoc,
                                               SubjectExpr,
                                               LBraceLoc,
                                               Cases.size(),
                                               RBraceLoc);
  memcpy(theSwitch->getCaseBuffer(),
         Cases.data(), Cases.size() * sizeof(CaseStmt*));
  return theSwitch;
}
