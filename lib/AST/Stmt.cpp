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

// Helper functions to check statically whether a method has been
// overridden from its implementation in Stmt.  The sort of thing you
// need when you're avoiding v-tables.
namespace {
  template <typename ReturnType, typename Class>
  constexpr bool isOverriddenFromStmt(ReturnType (Class::*)() const) {
    return true;
  }
  template <typename ReturnType>
  constexpr bool isOverriddenFromStmt(ReturnType (Stmt::*)() const) {
    return false;
  }

  template <bool IsOverridden> struct Dispatch;

  /// Dispatch down to a concrete override.
  template <> struct Dispatch<true> {
    template <class T> static SourceLoc getStartLoc(const T *S) {
      return S->getStartLoc();
    }
    template <class T> static SourceLoc getEndLoc(const T *S) {
      return S->getEndLoc();
    }
    template <class T> static SourceRange getSourceRange(const T *S) {
      return S->getSourceRange();
    }
  };

  /// Default implementations for when a method isn't overridden.
  template <> struct Dispatch<false> {
    template <class T> static SourceLoc getStartLoc(const T *S) {
      return S->getSourceRange().Start;
    }
    template <class T> static SourceLoc getEndLoc(const T *S) {
      return S->getSourceRange().End;
    }
    template <class T> static SourceRange getSourceRange(const T *S) {
      return { S->getStartLoc(), S->getEndLoc() };
    }
  };
}

template <class T> static SourceRange getSourceRangeImpl(const T *S) {
  static_assert(isOverriddenFromStmt(&T::getSourceRange) ||
                (isOverriddenFromStmt(&T::getStartLoc) &&
                 isOverriddenFromStmt(&T::getEndLoc)),
                "Stmt subclass must implement either getSourceRange() "
                "or getStartLoc()/getEndLoc()");
  return Dispatch<isOverriddenFromStmt(&T::getSourceRange)>::getSourceRange(S);
}

SourceRange Stmt::getSourceRange() const {
  switch (getKind()) {
#define STMT(ID, PARENT)                                           \
  case StmtKind::ID: return getSourceRangeImpl(cast<ID##Stmt>(this));
#include "swift/AST/StmtNodes.def"
  }
  
  llvm_unreachable("statement type not handled!");
}

template <class T> static SourceLoc getStartLocImpl(const T *S) {
  return Dispatch<isOverriddenFromStmt(&T::getStartLoc)>::getStartLoc(S);
}
SourceLoc Stmt::getStartLoc() const {
  switch (getKind()) {
#define STMT(ID, PARENT)                                           \
  case StmtKind::ID: return getStartLocImpl(cast<ID##Stmt>(this));
#include "swift/AST/StmtNodes.def"
  }

  llvm_unreachable("statement type not handled!");
}

template <class T> static SourceLoc getEndLocImpl(const T *S) {
  return Dispatch<isOverriddenFromStmt(&T::getEndLoc)>::getEndLoc(S);
}
SourceLoc Stmt::getEndLoc() const {
  switch (getKind()) {
#define STMT(ID, PARENT)                                           \
  case StmtKind::ID: return getEndLocImpl(cast<ID##Stmt>(this));
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

SourceLoc ReturnStmt::getStartLoc() const {
  if (ReturnLoc.isInvalid() && Result)
    return Result->getStartLoc();
  return ReturnLoc;
}
SourceLoc ReturnStmt::getEndLoc() const {
  return (Result ? Result->getEndLoc() : ReturnLoc);
}

SourceRange StmtConditionElement::getSourceRange() const {
  if (auto *E = getCondition())
    return E->getSourceRange();
  if (auto *B = getBinding())
    return B->getSourceRange();
  return SourceRange();
}

SourceLoc StmtConditionElement::getStartLoc() const {
  if (auto *E = getCondition())
    return E->getStartLoc();
  if (auto *B = getBinding())
    return B->getStartLoc();
  return SourceLoc();
}

SourceLoc StmtConditionElement::getEndLoc() const {
  if (auto *E = getCondition())
    return E->getEndLoc();
  if (auto *B = getBinding())
    return B->getEndLoc();
  return SourceLoc();
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

SourceLoc DoWhileStmt::getEndLoc() const { return Cond->getEndLoc(); }

SourceRange CaseLabelItem::getSourceRange() const {
  if (auto *E = getGuardExpr())
    return { CasePattern->getStartLoc(), E->getEndLoc() };
  return CasePattern->getSourceRange();
}
SourceLoc CaseLabelItem::getStartLoc() const {
  return CasePattern->getStartLoc();
}
SourceLoc CaseLabelItem::getEndLoc() const {
  if (auto *E = getGuardExpr())
    return E->getEndLoc();
  return CasePattern->getEndLoc();
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
