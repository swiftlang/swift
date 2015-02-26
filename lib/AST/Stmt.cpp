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
#include "swift/AST/Pattern.h"
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
  llvm_unreachable("bad StmtKind");
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

BraceStmt::BraceStmt(SourceLoc lbloc, ArrayRef<ASTNode> elts,
                     SourceLoc rbloc, Optional<bool> implicit)
  : Stmt(StmtKind::Brace, getDefaultImplicitFlag(implicit, lbloc)),
    NumElements(elts.size()), LBLoc(lbloc), RBLoc(rbloc)
{
  memcpy(getElementsStorage(), elts.data(),
         elts.size() * sizeof(ASTNode));
}

BraceStmt *BraceStmt::create(ASTContext &ctx, SourceLoc lbloc,
                             ArrayRef<ASTNode> elts, SourceLoc rbloc,
                             Optional<bool> implicit) {
  assert(std::none_of(elts.begin(), elts.end(),
                      [](ASTNode node) -> bool { return node.isNull(); }) &&
         "null element in BraceStmt");
  void *Buffer = ctx.Allocate(sizeof(BraceStmt)
                                + elts.size() * sizeof(ASTNode),
                              alignof(BraceStmt));
  return ::new(Buffer) BraceStmt(lbloc, elts, rbloc, implicit);
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

static StmtCondition exprToCond(Expr *C, ASTContext &Ctx) {
  StmtConditionElement Arr[] = { StmtConditionElement(C) };
  return Ctx.AllocateCopy(Arr);
}

IfStmt::IfStmt(SourceLoc IfLoc, Expr *Cond, Stmt *Then, SourceLoc ElseLoc,
               Stmt *Else, Optional<bool> implicit, ASTContext &Ctx)
  : IfStmt(LabeledStmtInfo(), IfLoc, exprToCond(Cond, Ctx), Then, ElseLoc, Else,
           implicit) {
}


SourceRange IfStmt::getSourceRange() const {
  SourceLoc End;
  if (Else)
    End = Else->getEndLoc();
  else
    End = Then->getEndLoc();
  return SourceRange(getLabelLocOrKeywordLoc(IfLoc), End);
}

SourceRange IfConfigStmt::getSourceRange() const {
  return SourceRange(getIfLoc(), EndLoc);
}

SourceRange WhileStmt::getSourceRange() const {
  return SourceRange(getLabelLocOrKeywordLoc(WhileLoc), Body->getEndLoc());
}

SourceRange DoWhileStmt::getSourceRange() const {
  return SourceRange(getLabelLocOrKeywordLoc(DoLoc), Cond->getEndLoc());
}

SourceRange ForStmt::getSourceRange() const {
  return SourceRange(getLabelLocOrKeywordLoc(ForLoc), Body->getEndLoc());
}

SourceRange ForEachStmt::getSourceRange() const {
  return SourceRange(getLabelLocOrKeywordLoc(ForLoc), Body->getEndLoc());
}

SourceRange SwitchStmt::getSourceRange() const {
  return {getLabelLocOrKeywordLoc(SwitchLoc), RBraceLoc};
}



SourceRange CaseLabelItem::getSourceRange() const {
  if (auto *E = getGuardExpr())
    return { CasePattern->getStartLoc(), E->getEndLoc() };
  return CasePattern->getSourceRange();
}

CaseStmt::CaseStmt(SourceLoc CaseLoc, ArrayRef<CaseLabelItem> CaseLabelItems,
                   bool HasBoundDecls, SourceLoc ColonLoc, Stmt *Body,
                   Optional<bool> Implicit)
    : Stmt(StmtKind::Case, getDefaultImplicitFlag(Implicit, CaseLoc)),
      CaseLoc(CaseLoc), ColonLoc(ColonLoc),
      BodyAndHasBoundDecls(Body, HasBoundDecls),
      NumPatterns(CaseLabelItems.size()) {
  assert(NumPatterns > 0 && "case block must have at least one pattern");
  MutableArrayRef<CaseLabelItem> Items{ getCaseLabelItemsBuffer(),
                                        NumPatterns };

  for (unsigned i = 0; i < NumPatterns; ++i) {
    new (&Items[i]) CaseLabelItem(CaseLabelItems[i]);
  }
}

CaseStmt *CaseStmt::create(ASTContext &C, SourceLoc CaseLoc,
                           ArrayRef<CaseLabelItem> CaseLabelItems,
                           bool HasBoundDecls, SourceLoc ColonLoc, Stmt *Body,
                           Optional<bool> Implicit) {
  void *Mem = C.Allocate(sizeof(CaseStmt) +
                             CaseLabelItems.size() * sizeof(CaseLabelItem),
                         alignof(CaseStmt));
  return ::new (Mem) CaseStmt(CaseLoc, CaseLabelItems, HasBoundDecls, ColonLoc,
                              Body, Implicit);
}

SwitchStmt *SwitchStmt::create(LabeledStmtInfo LabelInfo, SourceLoc SwitchLoc,
                               Expr *SubjectExpr,
                               SourceLoc LBraceLoc,
                               ArrayRef<CaseStmt *> Cases,
                               SourceLoc RBraceLoc,
                               ASTContext &C) {
  void *p = C.Allocate(sizeof(SwitchStmt) + Cases.size() * sizeof(SwitchStmt*),
                       alignof(SwitchStmt));
  SwitchStmt *theSwitch = ::new (p) SwitchStmt(LabelInfo, SwitchLoc,
                                               SubjectExpr, LBraceLoc,
                                               Cases.size(), RBraceLoc);
  memcpy(theSwitch->getCaseBuffer(),
         Cases.data(), Cases.size() * sizeof(CaseStmt*));
  return theSwitch;
}
