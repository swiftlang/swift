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
#include "swift/AST/Stmt.h"

using namespace swift::refactoring;

bool RefactoringActionConvertGuardExprToIfLetExpr::isApplicable(
    const ResolvedRangeInfo &Info, DiagnosticEngine &Diag) {
  if (Info.Kind != RangeKind::SingleStatement &&
      Info.Kind != RangeKind::MultiStatement)
    return false;

  if (Info.ContainedNodes.empty())
    return false;

  GuardStmt *guardStmt = nullptr;

  if (Info.ContainedNodes.size() > 0) {
    if (auto S = Info.ContainedNodes[0].dyn_cast<Stmt *>()) {
      guardStmt = dyn_cast<GuardStmt>(S);
    }
  }

  if (!guardStmt)
    return false;

  auto CondList = guardStmt->getCond();

  if (CondList.size() == 1) {
    auto E = CondList[0];
    auto P = E.getPatternOrNull();
    if (P && E.getKind() == swift::StmtConditionElement::CK_PatternBinding)
      return true;
  }

  return false;
}

bool RefactoringActionConvertGuardExprToIfLetExpr::performChange() {

  // Get guard stmt
  auto S = RangeInfo.ContainedNodes[0].dyn_cast<Stmt *>();
  GuardStmt *Guard = dyn_cast<GuardStmt>(S);

  // Get guard condition
  auto CondList = Guard->getCond();

  // Get guard condition source
  SourceRange range = CondList[0].getSourceRange();
  SourceManager &SM = RangeInfo.RangeContext->getASTContext().SourceMgr;
  auto CondCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, range);

  SmallString<64> DeclBuffer;
  llvm::raw_svector_ostream OS(DeclBuffer);

  StringRef Space = " ";
  StringRef NewLine = "\n";

  OS << tok::kw_if << Space;
  OS << CondCharRange.str().str() << Space;
  OS << tok::l_brace << NewLine;

  // Get nodes after guard to place them at if-let body
  if (RangeInfo.ContainedNodes.size() > 1) {
    auto S = RangeInfo.ContainedNodes[1].getSourceRange();
    S.widen(RangeInfo.ContainedNodes.back().getSourceRange());
    auto BodyCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, S);
    OS << BodyCharRange.str().str() << NewLine;
  }
  OS << tok::r_brace;

  // Get guard body
  auto Body = dyn_cast_or_null<BraceStmt>(Guard->getBody());

  if (Body && Body->getNumElements() > 1) {
    auto firstElement = Body->getFirstElement();
    auto lastElement = Body->getLastElement();
    SourceRange bodyRange = firstElement.getSourceRange();
    bodyRange.widen(lastElement.getSourceRange());
    auto BodyCharRange =
        Lexer::getCharSourceRangeFromSourceRange(SM, bodyRange);
    OS << Space << tok::kw_else << Space << tok::l_brace << NewLine;
    OS << BodyCharRange.str().str() << NewLine;
    OS << tok::r_brace;
  }

  // Replace guard to if-let
  auto ReplaceRange = RangeInfo.ContentRange;
  EditConsumer.accept(SM, ReplaceRange, DeclBuffer.str());

  return false;
}
