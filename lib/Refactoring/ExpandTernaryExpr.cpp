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
#include "swift/AST/Pattern.h"

using namespace swift::refactoring;

namespace {
/// Abstract helper class containing info about a TernaryExpr
/// that can be expanded into an IfStmt.
class ExpandableTernaryExprInfo {

public:
  virtual ~ExpandableTernaryExprInfo() {}

  virtual TernaryExpr *getTernary() = 0;

  virtual SourceRange getNameRange() = 0;

  virtual Type getType() = 0;

  virtual bool shouldDeclareNameAndType() { return !getType().isNull(); }

  virtual bool isValid() {

    // Ensure all public properties are non-nil and valid
    if (!getTernary() || !getNameRange().isValid())
      return false;
    if (shouldDeclareNameAndType() && getType().isNull())
      return false;

    return true; // valid
  }

  CharSourceRange getNameCharRange(const SourceManager &SM) {
    return Lexer::getCharSourceRangeFromSourceRange(SM, getNameRange());
  }
};

/// Concrete subclass containing info about an AssignExpr
/// where the source is the expandable TernaryExpr.
class ExpandableAssignTernaryExprInfo : public ExpandableTernaryExprInfo {

public:
  ExpandableAssignTernaryExprInfo(AssignExpr *Assign) : Assign(Assign) {}

  TernaryExpr *getTernary() override {
    if (!Assign)
      return nullptr;
    return dyn_cast_or_null<TernaryExpr>(Assign->getSrc());
  }

  SourceRange getNameRange() override {
    auto Invalid = SourceRange();

    if (!Assign)
      return Invalid;

    if (auto dest = Assign->getDest())
      return dest->getSourceRange();

    return Invalid;
  }

  Type getType() override { return nullptr; }

private:
  AssignExpr *Assign = nullptr;
};

/// Concrete subclass containing info about a PatternBindingDecl
/// where the pattern initializer is the expandable TernaryExpr.
class ExpandableBindingTernaryExprInfo : public ExpandableTernaryExprInfo {

public:
  ExpandableBindingTernaryExprInfo(PatternBindingDecl *Binding)
      : Binding(Binding) {}

  TernaryExpr *getTernary() override {
    if (Binding && Binding->getNumPatternEntries() == 1) {
      if (auto *Init = Binding->getInit(0)) {
        return dyn_cast<TernaryExpr>(Init);
      }
    }

    return nullptr;
  }

  SourceRange getNameRange() override {
    if (auto Pattern = getNamePattern())
      return Pattern->getSourceRange();

    return SourceRange();
  }

  Type getType() override {
    if (auto Pattern = getNamePattern())
      return Pattern->getType();

    return nullptr;
  }

private:
  Pattern *getNamePattern() {
    if (!Binding || Binding->getNumPatternEntries() != 1)
      return nullptr;

    auto Pattern = Binding->getPattern(0);

    if (!Pattern)
      return nullptr;

    if (auto TyPattern = dyn_cast<TypedPattern>(Pattern))
      Pattern = TyPattern->getSubPattern();

    return Pattern;
  }

  PatternBindingDecl *Binding = nullptr;
};

std::unique_ptr<ExpandableTernaryExprInfo>
findExpandableTernaryExpression(const ResolvedRangeInfo &Info) {

  if (Info.Kind != RangeKind::SingleDecl &&
      Info.Kind != RangeKind::SingleExpression)
    return nullptr;

  if (Info.ContainedNodes.size() != 1)
    return nullptr;

  if (auto D = Info.ContainedNodes[0].dyn_cast<Decl *>())
    if (auto Binding = dyn_cast<PatternBindingDecl>(D))
      return std::make_unique<ExpandableBindingTernaryExprInfo>(Binding);

  if (auto E = Info.ContainedNodes[0].dyn_cast<Expr *>())
    if (auto Assign = dyn_cast<AssignExpr>(E))
      return std::make_unique<ExpandableAssignTernaryExprInfo>(Assign);

  return nullptr;
}
} // namespace

bool RefactoringActionExpandTernaryExpr::isApplicable(
    const ResolvedRangeInfo &Info, DiagnosticEngine &Diag) {
  auto Target = findExpandableTernaryExpression(Info);
  return Target && Target->isValid();
}

bool RefactoringActionExpandTernaryExpr::performChange() {
  auto Target = findExpandableTernaryExpression(RangeInfo);

  if (!Target || !Target->isValid())
    return true; // abort

  auto NameCharRange = Target->getNameCharRange(SM);

  auto IfRange = Target->getTernary()->getSourceRange();
  auto IfCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, IfRange);

  auto CondRange = Target->getTernary()->getCondExpr()->getSourceRange();
  auto CondCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, CondRange);

  auto ThenRange = Target->getTernary()->getThenExpr()->getSourceRange();
  auto ThenCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, ThenRange);

  auto ElseRange = Target->getTernary()->getElseExpr()->getSourceRange();
  auto ElseCharRange = Lexer::getCharSourceRangeFromSourceRange(SM, ElseRange);

  SmallString<64> DeclBuffer;
  llvm::raw_svector_ostream OS(DeclBuffer);

  StringRef Space = " ";
  StringRef NewLine = "\n";

  if (Target->shouldDeclareNameAndType()) {
    // Specifier will not be replaced; append after specifier
    OS << NameCharRange.str() << tok::colon << Space;
    OS << Target->getType() << NewLine;
  }

  OS << tok::kw_if << Space;
  OS << CondCharRange.str() << Space;
  OS << tok::l_brace << NewLine;

  OS << NameCharRange.str() << Space;
  OS << tok::equal << Space;
  OS << ThenCharRange.str() << NewLine;

  OS << tok::r_brace << Space;
  OS << tok::kw_else << Space;
  OS << tok::l_brace << NewLine;

  OS << NameCharRange.str() << Space;
  OS << tok::equal << Space;
  OS << ElseCharRange.str() << NewLine;

  OS << tok::r_brace;

  // Start replacement with name range, skip the specifier
  auto ReplaceRange(NameCharRange);
  ReplaceRange.widen(IfCharRange);

  EditConsumer.accept(SM, ReplaceRange, DeclBuffer.str());

  return false; // don't abort
}
