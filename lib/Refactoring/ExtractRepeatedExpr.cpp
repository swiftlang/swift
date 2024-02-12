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

#include "ExtractExprBase.h"
#include "RefactoringActions.h"

using namespace swift::refactoring;

bool RefactoringActionExtractRepeatedExpr::isApplicable(
    const ResolvedRangeInfo &Info, DiagnosticEngine &Diag) {
  switch (Info.Kind) {
  case RangeKind::SingleExpression:
    return checkExtractConditions(Info, Diag)
        .success({CannotExtractReason::Literal});
  case RangeKind::PartOfExpression:
  case RangeKind::SingleDecl:
  case RangeKind::MultiTypeMemberDecl:
  case RangeKind::SingleStatement:
  case RangeKind::MultiStatement:
  case RangeKind::Invalid:
    return false;
  }
  llvm_unreachable("unhandled kind");
}

bool RefactoringActionExtractRepeatedExpr::performChange() {
  return RefactoringActionExtractExprBase(TheFile, RangeInfo, DiagEngine, true,
                                          PreferredName, EditConsumer)
      .performChange();
}
