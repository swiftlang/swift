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

using namespace swift::refactoring;

/// Given a cursor position, this function tries to collect a number literal
/// expression immediately following the cursor.
static NumberLiteralExpr *getTrailingNumberLiteral(ResolvedCursorInfoPtr Tok) {
  // This cursor must point to the start of an expression.
  auto ExprStartInfo = dyn_cast<ResolvedExprStartCursorInfo>(Tok);
  if (!ExprStartInfo)
    return nullptr;

  // For every sub-expression, try to find the literal expression that matches
  // our criteria.
  class FindLiteralNumber : public ASTWalker {
    Expr *const parent;

  public:
    NumberLiteralExpr *found = nullptr;

    explicit FindLiteralNumber(Expr *parent) : parent(parent) {}

    MacroWalking getMacroWalkingBehavior() const override {
      return MacroWalking::Arguments;
    }

    PreWalkResult<Expr *> walkToExprPre(Expr *expr) override {
      if (auto *literal = dyn_cast<NumberLiteralExpr>(expr)) {
        // The sub-expression must have the same start loc with the outermost
        // expression, i.e. the cursor position.
        if (!found && parent->getStartLoc().getOpaquePointerValue() ==
                          expr->getStartLoc().getOpaquePointerValue()) {
          found = literal;
        }
      }
      return Action::SkipNodeIf(found, expr);
    }
  };

  auto parent = ExprStartInfo->getTrailingExpr();
  FindLiteralNumber finder(parent);
  parent->walk(finder);
  return finder.found;
}

static std::string insertUnderscore(StringRef Text) {
  SmallString<64> Buffer;
  llvm::raw_svector_ostream OS(Buffer);
  for (auto It = Text.begin(); It != Text.end(); ++It) {
    unsigned Distance = It - Text.begin();
    if (Distance && !(Distance % 3)) {
      OS << '_';
    }
    OS << *It;
  }
  return OS.str().str();
}

void insertUnderscoreInDigits(StringRef Digits, raw_ostream &OS) {
  StringRef BeforePointRef, AfterPointRef;
  std::tie(BeforePointRef, AfterPointRef) = Digits.split('.');

  std::string BeforePoint(BeforePointRef);
  std::string AfterPoint(AfterPointRef);

  // Insert '_' for the part before the decimal point.
  std::reverse(BeforePoint.begin(), BeforePoint.end());
  BeforePoint = insertUnderscore(BeforePoint);
  std::reverse(BeforePoint.begin(), BeforePoint.end());
  OS << BeforePoint;

  // Insert '_' for the part after the decimal point, if necessary.
  if (!AfterPoint.empty()) {
    OS << '.';
    OS << insertUnderscore(AfterPoint);
  }
}

bool RefactoringActionSimplifyNumberLiteral::isApplicable(
    ResolvedCursorInfoPtr Tok, DiagnosticEngine &Diag) {
  if (auto *Literal = getTrailingNumberLiteral(Tok)) {
    SmallString<64> Buffer;
    llvm::raw_svector_ostream OS(Buffer);
    StringRef Digits = Literal->getDigitsText();
    insertUnderscoreInDigits(Digits, OS);

    // If inserting '_' results in a different digit sequence, this refactoring
    // is applicable.
    return OS.str() != Digits;
  }
  return false;
}

bool RefactoringActionSimplifyNumberLiteral::performChange() {
  if (auto *Literal = getTrailingNumberLiteral(CursorInfo)) {

    EditorConsumerInsertStream OS(
        EditConsumer, SM,
        CharSourceRange(SM, Literal->getDigitsLoc(),
                        Lexer::getLocForEndOfToken(SM, Literal->getEndLoc())));
    StringRef Digits = Literal->getDigitsText();
    insertUnderscoreInDigits(Digits, OS);
    return false;
  }
  return true;
}
