//===--- Stmt.cpp - Swift Language Statement ASTs -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "swift/Basic/Statistic.h"
#include "llvm/ADT/PointerUnion.h"

using namespace swift;

#define STMT(Id, _) \
  static_assert(IsTriviallyDestructible<Id##Stmt>::value, \
                "Stmts are BumpPtrAllocated; the destructor is never called");
#include "swift/AST/StmtNodes.def"

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
} // end anonymous namespace

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
    LBLoc(lbloc), RBLoc(rbloc)
{
  Bits.BraceStmt.NumElements = elts.size();
  std::uninitialized_copy(elts.begin(), elts.end(),
                          getTrailingObjects<ASTNode>());
}

BraceStmt *BraceStmt::create(ASTContext &ctx, SourceLoc lbloc,
                             ArrayRef<ASTNode> elts, SourceLoc rbloc,
                             Optional<bool> implicit) {
  assert(std::none_of(elts.begin(), elts.end(),
                      [](ASTNode node) -> bool { return node.isNull(); }) &&
         "null element in BraceStmt");
  void *Buffer = ctx.Allocate(totalSizeToAlloc<ASTNode>(elts.size()),
                              alignof(BraceStmt));
  return ::new(Buffer) BraceStmt(lbloc, elts, rbloc, implicit);
}

SourceLoc ReturnStmt::getStartLoc() const {
  if (ReturnLoc.isInvalid() && Result)
    return Result->getStartLoc();
  return ReturnLoc;
}
SourceLoc ReturnStmt::getEndLoc() const {
  if (Result && Result->getEndLoc().isValid())
    return Result->getEndLoc();
  return ReturnLoc;
}

YieldStmt *YieldStmt::create(const ASTContext &ctx, SourceLoc yieldLoc,
                             SourceLoc lpLoc, ArrayRef<Expr*> yields,
                             SourceLoc rpLoc, Optional<bool> implicit) {
  void *buffer = ctx.Allocate(totalSizeToAlloc<Expr*>(yields.size()),
                              alignof(YieldStmt));
  return ::new(buffer) YieldStmt(yieldLoc, lpLoc, yields, rpLoc, implicit);
}

SourceLoc YieldStmt::getEndLoc() const {
  return RPLoc.isInvalid() ? getYields()[0]->getEndLoc() : RPLoc;
}

SourceLoc ThrowStmt::getEndLoc() const { return SubExpr->getEndLoc(); }


SourceLoc DeferStmt::getEndLoc() const {
  return tempDecl->getBody()->getEndLoc();
}

/// Dig the original user's body of the defer out for AST fidelity.
BraceStmt *DeferStmt::getBodyAsWritten() const {
  return tempDecl->getBody();
}

bool LabeledStmt::isPossibleContinueTarget() const {
  switch (getKind()) {
#define LABELED_STMT(ID, PARENT)
#define STMT(ID, PARENT) case StmtKind::ID:
#include "swift/AST/StmtNodes.def"
    llvm_unreachable("not a labeled statement");

  // Sema has diagnostics with hard-coded expectations about what
  // statements return false from this method.
  case StmtKind::If:
  case StmtKind::Guard:
  case StmtKind::Switch:
    return false;

  case StmtKind::Do:
  case StmtKind::DoCatch:
  case StmtKind::RepeatWhile:
  case StmtKind::ForEach:
  case StmtKind::While:
    return true;
  }
  llvm_unreachable("statement kind unhandled!");
}

bool LabeledStmt::requiresLabelOnJump() const {
  switch (getKind()) {
#define LABELED_STMT(ID, PARENT)
#define STMT(ID, PARENT) case StmtKind::ID:
#include "swift/AST/StmtNodes.def"
    llvm_unreachable("not a labeled statement");

  case StmtKind::If:
  case StmtKind::Do:
  case StmtKind::DoCatch:
  case StmtKind::Guard: // Guard doesn't allow labels, so no break/continue.
    return true;

  case StmtKind::RepeatWhile:
  case StmtKind::ForEach:
  case StmtKind::Switch:
  case StmtKind::While:
    return false;
  }
  llvm_unreachable("statement kind unhandled!");
}

void ForEachStmt::setPattern(Pattern *p) {
  Pat = p;
  Pat->markOwnedByStatement(this);
}

void CatchStmt::setErrorPattern(Pattern *pattern) {
  ErrorPattern = pattern;
  ErrorPattern->markOwnedByStatement(this);
}


DoCatchStmt *DoCatchStmt::create(ASTContext &ctx, LabeledStmtInfo labelInfo,
                                 SourceLoc doLoc, Stmt *body,
                                 ArrayRef<CatchStmt*> catches,
                                 Optional<bool> implicit) {
  void *mem = ctx.Allocate(totalSizeToAlloc<CatchStmt*>(catches.size()),
                           alignof(DoCatchStmt));
  return ::new (mem) DoCatchStmt(labelInfo, doLoc, body, catches, implicit);
}

bool DoCatchStmt::isSyntacticallyExhaustive() const {
  for (auto clause : getCatches()) {
    if (clause->isSyntacticallyExhaustive())
      return true;
  }
  return false;
}

void LabeledConditionalStmt::setCond(StmtCondition e) {
  // When set a condition into a Conditional Statement, inform each of the
  // variables bound in any patterns that this is the owning statement for the
  // pattern.
  for (auto &elt : e)
    if (auto pat = elt.getPatternOrNull())
      pat->markOwnedByStatement(this);
  
  Cond = e;
}

bool CatchStmt::isSyntacticallyExhaustive() const {
  // It cannot have a guard expression and the pattern cannot be refutable.
  return getGuardExpr() == nullptr &&
         !getErrorPattern()->isRefutablePattern();
}


PoundAvailableInfo *PoundAvailableInfo::create(ASTContext &ctx,
                                               SourceLoc PoundLoc,
                                       ArrayRef<AvailabilitySpec *> queries,
                                                     SourceLoc RParenLoc) {
  unsigned size = totalSizeToAlloc<AvailabilitySpec *>(queries.size());
  void *Buffer = ctx.Allocate(size, alignof(PoundAvailableInfo));
  return ::new (Buffer) PoundAvailableInfo(PoundLoc, queries, RParenLoc);
}

SourceLoc PoundAvailableInfo::getEndLoc() const {
  if (RParenLoc.isInvalid()) {
    if (NumQueries == 0) {
      return PoundLoc;
    }
    return getQueries()[NumQueries - 1]->getSourceRange().End;
  }
  return RParenLoc;
}

SourceRange StmtConditionElement::getSourceRange() const {
  switch (getKind()) {
  case StmtConditionElement::CK_Boolean:
    return getBoolean()->getSourceRange();
  case StmtConditionElement::CK_Availability:
    return getAvailability()->getSourceRange();
  case StmtConditionElement::CK_PatternBinding:
    SourceLoc Start;
    if (IntroducerLoc.isValid())
      Start = IntroducerLoc;
    else
      Start = getPattern()->getStartLoc();
    
    SourceLoc End = getInitializer()->getEndLoc();
    if (Start.isValid() && End.isValid()) {
      return SourceRange(Start, End);
    } else {
      return SourceRange();
    }
  }

  llvm_unreachable("Unhandled StmtConditionElement in switch.");
}

SourceLoc StmtConditionElement::getStartLoc() const {
  switch (getKind()) {
  case StmtConditionElement::CK_Boolean:
    return getBoolean()->getStartLoc();
  case StmtConditionElement::CK_Availability:
    return getAvailability()->getStartLoc();
  case StmtConditionElement::CK_PatternBinding:
    return getSourceRange().Start;
  }

  llvm_unreachable("Unhandled StmtConditionElement in switch.");
}

SourceLoc StmtConditionElement::getEndLoc() const {
  switch (getKind()) {
  case StmtConditionElement::CK_Boolean:
    return getBoolean()->getEndLoc();
  case StmtConditionElement::CK_Availability:
    return getAvailability()->getEndLoc();
  case StmtConditionElement::CK_PatternBinding:
    return getSourceRange().End;
  }

  llvm_unreachable("Unhandled StmtConditionElement in switch.");
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

GuardStmt::GuardStmt(SourceLoc GuardLoc, Expr *Cond, Stmt *Body,
                     Optional<bool> implicit, ASTContext &Ctx)
  : GuardStmt(GuardLoc, exprToCond(Cond, Ctx), Body, implicit) {
    
}
  


SourceLoc RepeatWhileStmt::getEndLoc() const { return Cond->getEndLoc(); }

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
                   bool HasBoundDecls, SourceLoc UnknownAttrLoc,
                   SourceLoc ColonLoc, Stmt *Body, Optional<bool> Implicit)
    : Stmt(StmtKind::Case, getDefaultImplicitFlag(Implicit, CaseLoc)),
      UnknownAttrLoc(UnknownAttrLoc), CaseLoc(CaseLoc), ColonLoc(ColonLoc),
      BodyAndHasBoundDecls(Body, HasBoundDecls) {
  Bits.CaseStmt.NumPatterns = CaseLabelItems.size();
  assert(Bits.CaseStmt.NumPatterns > 0 &&
         "case block must have at least one pattern");
  MutableArrayRef<CaseLabelItem> Items{ getTrailingObjects<CaseLabelItem>(),
                                        Bits.CaseStmt.NumPatterns };

  for (unsigned i = 0; i < Bits.CaseStmt.NumPatterns; ++i) {
    new (&Items[i]) CaseLabelItem(CaseLabelItems[i]);
    Items[i].getPattern()->markOwnedByStatement(this);
  }
}

CaseStmt *CaseStmt::create(ASTContext &C, SourceLoc CaseLoc,
                           ArrayRef<CaseLabelItem> CaseLabelItems,
                           bool HasBoundDecls, SourceLoc UnknownAttrLoc,
                           SourceLoc ColonLoc, Stmt *Body,
                           Optional<bool> Implicit) {
  void *Mem = C.Allocate(totalSizeToAlloc<CaseLabelItem>(CaseLabelItems.size()),
                         alignof(CaseStmt));
  return ::new (Mem) CaseStmt(CaseLoc, CaseLabelItems, HasBoundDecls,
                              UnknownAttrLoc, ColonLoc, Body, Implicit);
}

SwitchStmt *SwitchStmt::create(LabeledStmtInfo LabelInfo, SourceLoc SwitchLoc,
                               Expr *SubjectExpr,
                               SourceLoc LBraceLoc,
                               ArrayRef<ASTNode> Cases,
                               SourceLoc RBraceLoc,
                               ASTContext &C) {
#ifndef NDEBUG
  for (auto N : Cases)
    assert((N.is<Stmt*>() && isa<CaseStmt>(N.get<Stmt*>())) ||
           (N.is<Decl*>() && (isa<IfConfigDecl>(N.get<Decl*>()) ||
                              isa<PoundDiagnosticDecl>(N.get<Decl*>()))));
#endif

  void *p = C.Allocate(totalSizeToAlloc<ASTNode>(Cases.size()),
                       alignof(SwitchStmt));
  SwitchStmt *theSwitch = ::new (p) SwitchStmt(LabelInfo, SwitchLoc,
                                               SubjectExpr, LBraceLoc,
                                               Cases.size(), RBraceLoc);

  std::uninitialized_copy(Cases.begin(), Cases.end(),
                          theSwitch->getTrailingObjects<ASTNode>());
  return theSwitch;
}

// See swift/Basic/Statistic.h for declaration: this enables tracing Stmts, is
// defined here to avoid too much layering violation / circular linkage
// dependency.

struct StmtTraceFormatter : public UnifiedStatsReporter::TraceFormatter {
  void traceName(const void *Entity, raw_ostream &OS) const {
    if (!Entity)
      return;
    const Stmt *S = static_cast<const Stmt *>(Entity);
    OS << Stmt::getKindName(S->getKind());
  }
  void traceLoc(const void *Entity, SourceManager *SM,
                clang::SourceManager *CSM, raw_ostream &OS) const {
    if (!Entity)
      return;
    const Stmt *S = static_cast<const Stmt *>(Entity);
    S->getSourceRange().print(OS, *SM, false);
  }
};

static StmtTraceFormatter TF;

template<>
const UnifiedStatsReporter::TraceFormatter*
FrontendStatsTracer::getTraceFormatter<const Stmt *>() {
  return &TF;
}
