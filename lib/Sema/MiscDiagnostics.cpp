//===--- MiscDiagnostics.cpp - AST-Level Diagnostics ----------------------===//
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
// This file implements AST-level diagnostics.
//
//===----------------------------------------------------------------------===//

#include "MiscDiagnostics.h"
#include "TypeChecker.h"
#include "swift/Basic/SourceManager.h"

using namespace swift;

//===--------------------------------------------------------------------===//
// Diagnose assigning variable to itself.
//===--------------------------------------------------------------------===//

static Decl *findSimpleReferencedDecl(const Expr *E) {
  if (auto *LE = dyn_cast<LoadExpr>(E))
    E = LE->getSubExpr();

  if (auto *DRE = dyn_cast<DeclRefExpr>(E))
    return DRE->getDecl();

  return nullptr;
}

static std::pair<Decl *, Decl *> findReferencedDecl(const Expr *E) {
  if (auto *LE = dyn_cast<LoadExpr>(E))
    E = LE->getSubExpr();

  if (auto *D = findSimpleReferencedDecl(E))
    return std::make_pair(nullptr, D);

  if (auto *MRE = dyn_cast<MemberRefExpr>(E)) {
    if (auto *BaseDecl = findSimpleReferencedDecl(MRE->getBase()))
      return std::make_pair(BaseDecl, MRE->getMember().getDecl());
  }

  return std::make_pair(nullptr, nullptr);
}

static void diagSelfAssignment(TypeChecker &TC, const Expr *E) {
  auto *AE = dyn_cast<AssignExpr>(E);
  if (!AE)
    return;

  auto LHSDecl = findReferencedDecl(AE->getDest());
  auto RHSDecl = findReferencedDecl(AE->getSrc());
  if (LHSDecl.second && LHSDecl == RHSDecl) {
    TC.diagnose(AE->getLoc(), LHSDecl.first ? diag::self_assignment_prop
                                            : diag::self_assignment_var)
        .highlight(AE->getDest()->getSourceRange())
        .highlight(AE->getSrc()->getSourceRange());
  }
}

//===--------------------------------------------------------------------===//
// Diagnose unreachable code.
//===--------------------------------------------------------------------===//

/// Issue a warning on code containing retrun expression on a differnt line than
/// the return keyword and both have the same indentation:
///  ...
///  return
///  foo()
static void diagUnreachableCode(TypeChecker &TC, const Stmt *S) {
  auto *RS = dyn_cast<ReturnStmt>(S);
  if (!RS)
    return;
  if (!RS->hasResult())
    return;

  auto RetExpr = RS->getResult();
  auto RSLoc = RS->getStartLoc();
  auto RetExprLoc = RetExpr->getStartLoc();
  // FIXME: Expose getColumnNumber() in LLVM SourceMgr to make this check
  // cheaper.
  if (RSLoc.isInvalid() || RetExprLoc.isInvalid() || (RSLoc == RetExprLoc))
    return;
  SourceManager &SM = TC.Context.SourceMgr;
  if (SM.getLineAndColumn(RSLoc).second ==
      SM.getLineAndColumn(RetExprLoc).second) {
    TC.diagnose(RetExpr->getStartLoc(), diag::unindented_code_after_return);
    TC.diagnose(RetExpr->getStartLoc(), diag::indent_expression_to_silence);
    return;
  }
  return;

}

//===--------------------------------------------------------------------===//
// High-level entry points.
//===--------------------------------------------------------------------===//

void swift::performExprDiagnostics(TypeChecker &TC, const Expr *E) {
  diagSelfAssignment(TC, E);
}

void swift::performStmtDiagnostics(TypeChecker &TC, const Stmt *S) {
  return diagUnreachableCode(TC, S);
}

