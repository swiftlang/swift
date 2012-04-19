//===--- CaptureAnalysis.cpp - Analyze capture properties -----------------===//
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
// This file implements analysis for local variables and expressions which can
// capture them, to optimize code generation.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"

using namespace swift;

static void VisitValueDecl(ValueDecl *VD) {
  if (VD->getDeclContext()->isLocalContext()) {
    // We assume that these flags are correct unless
    // we show otherwise in walkToExprPre.
    VD->setNeverUsedAsLValue(true);
    VD->setHasFixedLifetime(true);
  }
}

// Find ValueDecls in the given pattern.
static void WalkPattern(Pattern *P) {
  switch (P->getKind()) {
  case PatternKind::Tuple:
    for (auto &field : cast<TuplePattern>(P)->getFields())
      WalkPattern(field.getPattern());
    return;

  case PatternKind::Paren:
    return WalkPattern(cast<ParenPattern>(P)->getSubPattern());

  case PatternKind::Typed:
    return WalkPattern(cast<TypedPattern>(P)->getSubPattern());

  case PatternKind::Named:
    VisitValueDecl(cast<NamedPattern>(P)->getDecl());
    break;

  case PatternKind::Any:
    break;
  }
}

static ValueDecl* FindValueDecl(Expr *E) {
  // Strip off expressions which don't matter for this analysis.
  while (1) {
    if (ParenExpr *PE = dyn_cast<ParenExpr>(E))
      E = PE->getSubExpr();
    else if (TupleElementExpr *TE = dyn_cast<TupleElementExpr>(E))
      E = TE->getBase();
    else if (LookThroughOneofExpr *LTOE = dyn_cast<LookThroughOneofExpr>(E))
      E = LTOE->getSubExpr();
    else if (AddressOfExpr *AOE = dyn_cast<AddressOfExpr>(E))
      E = AOE->getSubExpr();
    else
      break;
  }
  // Return the found DeclRefExpr.
  if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E))
    return DRE->getDecl();
  return 0;
}

namespace {
// This recursive visitor implements two rules:
//
// 1. A local variable's lifetime is fixed (i.e. the variable can be
//    emitted on the stack) if it isn't captured by a CapturingExpr
//    and all DeclRefExprs are operands of known-safe operations
//    (specifically, LoadExprs, RequalifyExprs which strip off the
//    heap qualifier, and AssignStmts).
//
// 2. A DeclRefExpr referring to a variable is an "lvalue use" if it is not
//    the operand of a LoadExpr.
class CaptureAnalysisVisitor : public ASTWalker {
  bool walkToExprPre(Expr *E) {
    if (LoadExpr *LE = dyn_cast<LoadExpr>(E)) {
      // A DeclRefExpr which is immediately loaded can't extend the lifetime of
      // a variable, and can't modify it.
      if (FindValueDecl(LE->getSubExpr()))
        return false;
    } else if (RequalifyExpr *RE = dyn_cast<RequalifyExpr>(E)) {
      // A DeclRefExpr which has the heap qualifier stripped off can't extend
      // the lifetime of a variable.
      LValueType *SrcLT = RE->getSubExpr()->getType()->castTo<LValueType>();
      LValueType *DstLT = RE->getType()->castTo<LValueType>();
      if ((DstLT->getQualifiers() & LValueType::Qual::NonHeap) &&
          !(SrcLT->getQualifiers() & LValueType::Qual::NonHeap))
      if (ValueDecl *D = FindValueDecl(RE->getSubExpr())) {
        if (D->getDeclContext()->isLocalContext())
          D->setNeverUsedAsLValue(false);
        return false;
      }
    } else if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
      // We can't reason about the decl referred to by a general DeclRefExpr.
      ValueDecl *D = DRE->getDecl();
      if (D->getDeclContext()->isLocalContext()) {
        D->setNeverUsedAsLValue(false);
        D->setHasFixedLifetime(false);
      }
    } else if (CapturingExpr *CE = dyn_cast<CapturingExpr>(E)) {
      // Initialize flags for the function arguments.
      if (FuncExpr *FE = dyn_cast<FuncExpr>(CE)) {
        for (Pattern *P : FE->getParamPatterns())
          WalkPattern(P);
      } else if (ClosureExpr *CLE = dyn_cast<ClosureExpr>(CE)) {
        WalkPattern(CLE->getPattern());
      }
      // A variable which is captured might have its lifetime extended.
      // FIXME: We can be a lot smarter here if we prove that either the
      // CapturingExpr can't be captured or the variable can't be modified
      // after it is captured.  Proving a CapturingExpr can't be captured
      // requires nocapture annotations.  IRGen can use the neverUsedAsLValue
      // as a trivial form of proving a variable can't be modified; control
      // flow analysis plus a way to force IRGen to copy a variable when
      // a given expression captures it would allow some more elaborate
      // tricks here.
      for (ValueDecl *D : CE->getCaptures())
        D->setHasFixedLifetime(false);
    }
    return true;
  }

  bool walkToStmtPre(Stmt *S) {
    if (AssignStmt *AS = dyn_cast<AssignStmt>(S)) {
      // An assignment to a variable can't extend its lifetime.
      if (ValueDecl *D = FindValueDecl(AS->getDest())) {
        if (D->getDeclContext()->isLocalContext())
          D->setNeverUsedAsLValue(false);

        AS->getSrc()->walk(*this);
        return false;
      }
    }
    return true;
  }

  bool walkToDeclPre(Decl *D) {
    if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
      VisitValueDecl(VD);

    return true;
  }
};

} // end anonymous namespace

void swift::performCaptureAnalysis(TranslationUnit *TU, unsigned StartElem) {
  CaptureAnalysisVisitor walker;
  for (unsigned i = StartElem, e = TU->Body->getNumElements(); i != e; ++i) {
    auto Elem = TU->Body->getElement(i);
    if (Expr *E = Elem.dyn_cast<Expr*>())
      E->walk(walker);
    else if (Stmt *S = Elem.dyn_cast<Stmt*>())
      S->walk(walker);
    else
      Elem.get<Decl*>()->walk(walker);
  }
}
