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

/// Struct containing info about an IfStmt that can be converted into a
/// TernaryExpr.
struct ConvertToTernaryExprInfo {
  ConvertToTernaryExprInfo() {}

  Expr *AssignDest() {

    if (!Then || !Then->getDest() || !Else || !Else->getDest())
      return nullptr;

    auto ThenDest = Then->getDest();
    auto ElseDest = Else->getDest();

    if (ThenDest->getKind() != ElseDest->getKind())
      return nullptr;

    switch (ThenDest->getKind()) {
    case ExprKind::DeclRef: {
      auto ThenRef = dyn_cast<DeclRefExpr>(Then->getDest());
      auto ElseRef = dyn_cast<DeclRefExpr>(Else->getDest());

      if (!ThenRef || !ThenRef->getDecl() || !ElseRef || !ElseRef->getDecl())
        return nullptr;

      const auto ThenName = ThenRef->getDecl()->getName();
      const auto ElseName = ElseRef->getDecl()->getName();

      if (ThenName.compare(ElseName) != 0)
        return nullptr;

      return Then->getDest();
    }
    case ExprKind::Tuple: {
      auto ThenTuple = dyn_cast<TupleExpr>(Then->getDest());
      auto ElseTuple = dyn_cast<TupleExpr>(Else->getDest());

      if (!ThenTuple || !ElseTuple)
        return nullptr;

      auto ThenNames = ThenTuple->getElementNames();
      auto ElseNames = ElseTuple->getElementNames();

      if (!ThenNames.equals(ElseNames))
        return nullptr;

      return ThenTuple;
    }
    default:
      return nullptr;
    }
  }

  Expr *ThenSrc() {
    if (!Then)
      return nullptr;
    return Then->getSrc();
  }

  Expr *ElseSrc() {
    if (!Else)
      return nullptr;
    return Else->getSrc();
  }

  bool isValid() {
    if (!Cond || !AssignDest() || !ThenSrc() || !ElseSrc() ||
        !IfRange.isValid())
      return false;

    return true;
  }

  PatternBindingDecl *Binding = nullptr; // optional

  Expr *Cond = nullptr;       // required
  AssignExpr *Then = nullptr; // required
  AssignExpr *Else = nullptr; // required
  SourceRange IfRange;
};

ConvertToTernaryExprInfo
findConvertToTernaryExpression(const ResolvedRangeInfo &Info) {

  auto notFound = ConvertToTernaryExprInfo();

  if (Info.Kind != RangeKind::SingleStatement &&
      Info.Kind != RangeKind::MultiStatement)
    return notFound;

  if (Info.ContainedNodes.empty())
    return notFound;

  struct AssignExprFinder : public SourceEntityWalker {

    AssignExpr *Assign = nullptr;

    AssignExprFinder(Stmt *S) {
      if (S)
        walk(S);
    }

    virtual bool walkToExprPre(Expr *E) override {
      Assign = dyn_cast<AssignExpr>(E);
      return false;
    }
  };

  ConvertToTernaryExprInfo Target;

  IfStmt *If = nullptr;

  if (Info.ContainedNodes.size() == 1) {
    if (auto S = Info.ContainedNodes[0].dyn_cast<Stmt *>())
      If = dyn_cast<IfStmt>(S);
  }

  if (Info.ContainedNodes.size() == 2) {
    if (auto D = Info.ContainedNodes[0].dyn_cast<Decl *>())
      Target.Binding = dyn_cast<PatternBindingDecl>(D);
    if (auto S = Info.ContainedNodes[1].dyn_cast<Stmt *>())
      If = dyn_cast<IfStmt>(S);
  }

  if (!If)
    return notFound;

  auto CondList = If->getCond();

  if (CondList.size() != 1)
    return notFound;

  Target.Cond = CondList[0].getBooleanOrNull();
  Target.IfRange = If->getSourceRange();

  Target.Then = AssignExprFinder(If->getThenStmt()).Assign;
  Target.Else = AssignExprFinder(If->getElseStmt()).Assign;

  return Target;
}

bool RefactoringActionConvertToTernaryExpr::isApplicable(
    const ResolvedRangeInfo &Info, DiagnosticEngine &Diag) {
  return findConvertToTernaryExpression(Info).isValid();
}

bool RefactoringActionConvertToTernaryExpr::performChange() {
  auto Target = findConvertToTernaryExpression(RangeInfo);

  if (!Target.isValid())
    return true; // abort

  SmallString<64> DeclBuffer;
  llvm::raw_svector_ostream OS(DeclBuffer);

  StringRef Space = " ";

  auto IfRange = Target.IfRange;
  auto ReplaceRange = Lexer::getCharSourceRangeFromSourceRange(SM, IfRange);

  auto CondRange = Target.Cond->getSourceRange();
  auto CondCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, CondRange);

  auto ThenRange = Target.ThenSrc()->getSourceRange();
  auto ThenCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, ThenRange);

  auto ElseRange = Target.ElseSrc()->getSourceRange();
  auto ElseCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, ElseRange);

  CharSourceRange DestCharRange;

  if (Target.Binding) {
    auto DestRange = Target.Binding->getSourceRange();
    DestCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, DestRange);
    ReplaceRange.widen(DestCharRange);
  } else {
    auto DestRange = Target.AssignDest()->getSourceRange();
    DestCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, DestRange);
  }

  OS << DestCharRange.str() << Space << tok::equal << Space;
  OS << CondCharRange.str() << Space << tok::question_postfix << Space;
  OS << ThenCharRange.str() << Space << tok::colon << Space;
  OS << ElseCharRange.str();

  EditConsumer.accept(SM, ReplaceRange, DeclBuffer.str());

  return false; // don't abort
}
