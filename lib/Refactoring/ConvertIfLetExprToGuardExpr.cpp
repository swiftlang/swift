//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "RefactoringActions.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"

using namespace swift::refactoring;

bool RefactoringActionConvertIfLetExprToGuardExpr::isApplicable(
    const ResolvedRangeInfo &Info, DiagnosticEngine &Diag) {

  if (Info.Kind != RangeKind::SingleStatement &&
      Info.Kind != RangeKind::MultiStatement)
    return false;

  if (Info.ContainedNodes.empty())
    return false;

  IfStmt *If = nullptr;

  if (Info.ContainedNodes.size() == 1) {
    if (auto S = Info.ContainedNodes[0].dyn_cast<Stmt *>()) {
      If = dyn_cast<IfStmt>(S);
    }
  }

  if (!If)
    return false;

  auto CondList = If->getCond();

  if (CondList.size() == 1) {
    auto E = CondList[0];
    auto P = E.getKind();
    if (P == swift::StmtConditionElement::CK_PatternBinding) {
      auto Body = dyn_cast_or_null<BraceStmt>(If->getThenStmt());
      if (Body)
        return true;
    }
  }

  return false;
}

bool RefactoringActionConvertIfLetExprToGuardExpr::performChange() {

  auto S = RangeInfo.ContainedNodes[0].dyn_cast<Stmt *>();
  IfStmt *If = dyn_cast<IfStmt>(S);
  auto CondList = If->getCond();

  // Get if-let condition
  SourceRange range = CondList[0].getSourceRange();
  SourceManager &SM = RangeInfo.RangeContext->getASTContext().SourceMgr;
  auto CondCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, range);

  auto Body = dyn_cast_or_null<BraceStmt>(If->getThenStmt());

  // Get if-let then body.
  auto firstElement = Body->getFirstElement();
  auto lastElement = Body->getLastElement();
  SourceRange bodyRange = firstElement.getSourceRange();
  bodyRange.widen(lastElement.getSourceRange());
  auto BodyCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, bodyRange);

  SmallString<64> DeclBuffer;
  llvm::raw_svector_ostream OS(DeclBuffer);

  StringRef Space = " ";
  StringRef NewLine = "\n";

  OS << tok::kw_guard << Space;
  OS << CondCharRange.str().str() << Space;
  OS << tok::kw_else << Space;
  OS << tok::l_brace << NewLine;

  // Get if-let else body.
  if (auto *ElseBody = dyn_cast_or_null<BraceStmt>(If->getElseStmt())) {
    auto firstElseElement = ElseBody->getFirstElement();
    auto lastElseElement = ElseBody->getLastElement();
    SourceRange elseBodyRange = firstElseElement.getSourceRange();
    elseBodyRange.widen(lastElseElement.getSourceRange());
    auto ElseBodyCharRange =
        Lexer::getCharSourceRangeFromSourceRange(SM, elseBodyRange);
    OS << ElseBodyCharRange.str().str() << NewLine;
  }

  OS << tok::kw_return << NewLine;
  OS << tok::r_brace << NewLine;
  OS << BodyCharRange.str().str();

  // Replace if-let to guard
  auto ReplaceRange = RangeInfo.ContentRange;
  EditConsumer.accept(SM, ReplaceRange, DeclBuffer.str());

  return false;
}
