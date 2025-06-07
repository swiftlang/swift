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

static Expr *findLocalizeTarget(ResolvedCursorInfoPtr CursorInfo) {
  auto ExprStartInfo = dyn_cast<ResolvedExprStartCursorInfo>(CursorInfo);
  if (!ExprStartInfo)
    return nullptr;
  struct StringLiteralFinder : public SourceEntityWalker {
    SourceLoc StartLoc;
    Expr *Target;
    StringLiteralFinder(SourceLoc StartLoc)
        : StartLoc(StartLoc), Target(nullptr) {}
    bool walkToExprPre(Expr *E) override {
      if (E->getStartLoc() != StartLoc)
        return false;
      if (E->getKind() == ExprKind::InterpolatedStringLiteral)
        return false;
      if (E->getKind() == ExprKind::StringLiteral) {
        Target = E;
        return false;
      }
      return true;
    }
  } Walker(ExprStartInfo->getTrailingExpr()->getStartLoc());
  Walker.walk(ExprStartInfo->getTrailingExpr());
  return Walker.Target;
}

bool RefactoringActionLocalizeString::isApplicable(ResolvedCursorInfoPtr Tok,
                                                   DiagnosticEngine &Diag) {
  return findLocalizeTarget(Tok);
}

bool RefactoringActionLocalizeString::performChange() {
  Expr *Target = findLocalizeTarget(CursorInfo);
  if (!Target)
    return true;
  EditConsumer.accept(SM, Target->getStartLoc(), "NSLocalizedString(");
  EditConsumer.insertAfter(SM, Target->getEndLoc(), ", comment: \"\")");
  return false;
}
