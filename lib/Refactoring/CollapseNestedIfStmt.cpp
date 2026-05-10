//===----------------------------------------------------------------------===//
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

#include "RefactoringActions.h"
#include "swift/AST/Stmt.h"

using namespace swift::refactoring;

static std::pair<IfStmt *, IfStmt *>
findCollapseNestedIfTarget(ResolvedCursorInfoPtr CursorInfo) {
  auto StmtStartInfo = dyn_cast<ResolvedStmtStartCursorInfo>(CursorInfo);
  if (!StmtStartInfo)
    return {};

  // Ensure the statement is 'if' statement. It must not have 'else' clause.
  IfStmt *OuterIf = dyn_cast<IfStmt>(StmtStartInfo->getTrailingStmt());
  if (!OuterIf)
    return {};
  if (OuterIf->getElseStmt())
    return {};

  // The body must contain a sole inner 'if' statement.
  auto Body = dyn_cast_or_null<BraceStmt>(OuterIf->getThenStmt());
  if (!Body || Body->getNumElements() != 1)
    return {};

  IfStmt *InnerIf =
      dyn_cast_or_null<IfStmt>(Body->getFirstElement().dyn_cast<Stmt *>());
  if (!InnerIf)
    return {};

  // Inner 'if' statement also cannot have 'else' clause.
  if (InnerIf->getElseStmt())
    return {};

  return {OuterIf, InnerIf};
}

bool RefactoringActionCollapseNestedIfStmt::isApplicable(
    ResolvedCursorInfoPtr CursorInfo, DiagnosticEngine &Diag) {
  return findCollapseNestedIfTarget(CursorInfo).first;
}

bool RefactoringActionCollapseNestedIfStmt::performChange() {
  auto Target = findCollapseNestedIfTarget(CursorInfo);
  if (!Target.first)
    return true;
  auto OuterIf = Target.first;
  auto InnerIf = Target.second;

  EditorConsumerInsertStream OS(
      EditConsumer, SM,
      Lexer::getCharSourceRangeFromSourceRange(SM, OuterIf->getSourceRange()));

  OS << tok::kw_if << " ";

  // Emit conditions.
  bool first = true;
  for (auto &C : llvm::concat<StmtConditionElement>(OuterIf->getCond(),
                                                    InnerIf->getCond())) {
    if (first)
      first = false;
    else
      OS << ", ";
    OS << Lexer::getCharSourceRangeFromSourceRange(SM, C.getSourceRange())
              .str();
  }

  // Emit body.
  OS << " ";
  OS << Lexer::getCharSourceRangeFromSourceRange(
            SM, InnerIf->getThenStmt()->getSourceRange())
            .str();
  return false;
}
