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

/// Get the source file that corresponds to the given buffer.
SourceFile *getContainingFile(ModuleDecl *M, RangeConfig Range) {
  auto &SM = M->getASTContext().SourceMgr;
  // TODO: We should add an ID -> SourceFile mapping.
  return M->getSourceFileContainingLocation(
      SM.getRangeForBuffer(Range.BufferID).getStart());
}

RefactoringAction::RefactoringAction(ModuleDecl *MD, RefactoringOptions &Opts,
                                     SourceEditConsumer &EditConsumer,
                                     DiagnosticConsumer &DiagConsumer)
    : MD(MD), TheFile(getContainingFile(MD, Opts.Range)),
      EditConsumer(EditConsumer), Ctx(MD->getASTContext()),
      SM(MD->getASTContext().SourceMgr), DiagEngine(SM),
      StartLoc(Lexer::getLocForStartOfToken(SM, Opts.Range.getStart(SM))),
      PreferredName(Opts.PreferredName) {
  DiagEngine.addConsumer(DiagConsumer);
}

TokenBasedRefactoringAction::TokenBasedRefactoringAction(
    ModuleDecl *MD, RefactoringOptions &Opts, SourceEditConsumer &EditConsumer,
    DiagnosticConsumer &DiagConsumer)
    : RefactoringAction(MD, Opts, EditConsumer, DiagConsumer) {
  // Resolve the sema token and save it for later use.
  CursorInfo =
      evaluateOrDefault(TheFile->getASTContext().evaluator,
                        CursorInfoRequest{CursorInfoOwner(TheFile, StartLoc)},
                        new ResolvedCursorInfo());
}

RangeBasedRefactoringAction::RangeBasedRefactoringAction(
    ModuleDecl *MD, RefactoringOptions &Opts, SourceEditConsumer &EditConsumer,
    DiagnosticConsumer &DiagConsumer)
    : RefactoringAction(MD, Opts, EditConsumer, DiagConsumer),
      RangeInfo(evaluateOrDefault(
          MD->getASTContext().evaluator,
          RangeInfoRequest(RangeInfoOwner(TheFile, Opts.Range.getStart(SM),
                                          Opts.Range.getEnd(SM))),
          ResolvedRangeInfo())) {}
