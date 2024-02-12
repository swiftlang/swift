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

using namespace swift::refactoring;

namespace {
// A SingleDecl range may not include all decls actually declared in that range:
// a var decl has accessors that aren't included. This will find those missing
// decls.
class FindAllSubDecls : public SourceEntityWalker {
  SmallPtrSetImpl<Decl *> &Found;

public:
  FindAllSubDecls(SmallPtrSetImpl<Decl *> &found) : Found(found) {}

  bool walkToDeclPre(Decl *D, CharSourceRange range) override {
    // Record this Decl, and skip its contents if we've already touched it.
    if (!Found.insert(D).second)
      return false;

    if (auto ASD = dyn_cast<AbstractStorageDecl>(D)) {
      ASD->visitParsedAccessors(
          [&](AccessorDecl *accessor) { Found.insert(accessor); });
    }
    return true;
  }
};
} // namespace

bool RefactoringActionReplaceBodiesWithFatalError::isApplicable(
    const ResolvedRangeInfo &Info, DiagnosticEngine &Diag) {
  switch (Info.Kind) {
  case RangeKind::SingleDecl:
  case RangeKind::MultiTypeMemberDecl: {
    SmallPtrSet<Decl *, 16> Found;
    for (auto decl : Info.DeclaredDecls) {
      FindAllSubDecls(Found).walk(decl.VD);
    }
    for (auto decl : Found) {
      auto AFD = dyn_cast<AbstractFunctionDecl>(decl);
      if (AFD && !AFD->isImplicit())
        return true;
    }

    return false;
  }
  case RangeKind::SingleExpression:
  case RangeKind::PartOfExpression:
  case RangeKind::SingleStatement:
  case RangeKind::MultiStatement:
  case RangeKind::Invalid:
    return false;
  }
  llvm_unreachable("unhandled kind");
}

bool RefactoringActionReplaceBodiesWithFatalError::performChange() {
  const StringRef replacement = "{\nfatalError()\n}";
  SmallPtrSet<Decl *, 16> Found;
  for (auto decl : RangeInfo.DeclaredDecls) {
    FindAllSubDecls(Found).walk(decl.VD);
  }
  for (auto decl : Found) {
    auto AFD = dyn_cast<AbstractFunctionDecl>(decl);
    if (!AFD || AFD->isImplicit())
      continue;

    auto range = AFD->getBodySourceRange();
    // If we're in replacement mode (i.e. have an edit consumer), we can
    // rewrite the function body.
    auto charRange = Lexer::getCharSourceRangeFromSourceRange(SM, range);
    EditConsumer.accept(SM, charRange, replacement);
  }
  return false;
}
