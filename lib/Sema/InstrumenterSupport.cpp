//===--- InstrumenterSupport.cpp - Instrumenter Support -------------------===//
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
//
//  This file implements the supporting functions for writing instrumenters of
//  the Swift AST.
//
//===----------------------------------------------------------------------===//

#include "InstrumenterSupport.h"
#include "swift/AST/DiagnosticSuppression.h"

using namespace swift;
using namespace swift::instrumenter_support;

namespace {

class ErrorGatherer : public DiagnosticConsumer {
private:
  bool error = false;
  DiagnosticEngine &diags;

public:
  ErrorGatherer(DiagnosticEngine &diags) : diags(diags) {
    diags.addConsumer(*this);
  }
  ~ErrorGatherer() override { diags.takeConsumers(); }
  void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                        DiagnosticKind Kind,
                        StringRef FormatString,
                        ArrayRef<DiagnosticArgument> FormatArgs,
                        const DiagnosticInfo &Info) override {
    if (Kind == swift::DiagnosticKind::Error) {
      error = true;
    }
    DiagnosticEngine::formatDiagnosticText(llvm::errs(), FormatString,
                                           FormatArgs);
    llvm::errs() << "\n";
  }
  bool hadError() { return error; }
};


class ErrorFinder : public ASTWalker {
  bool error = false;

public:
  ErrorFinder() {}
  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    if (isa<ErrorExpr>(E) || !E->getType() || E->getType()->hasError()) {
      error = true;
      return {false, E};
    }
    return {true, E};
  }
  bool walkToDeclPre(Decl *D) override {
    if (auto *VD = dyn_cast<ValueDecl>(D)) {
      if (!VD->hasInterfaceType() || VD->getInterfaceType()->hasError()) {
        error = true;
        return false;
      }
    }
    return true;
  }
  bool hadError() { return error; }
};
} // end anonymous namespace

void InstrumenterBase::anchor() {}

bool InstrumenterBase::doTypeCheckImpl(ASTContext &Ctx, DeclContext *DC,
                                       Expr * &parsedExpr) {
  DiagnosticSuppression suppression(Ctx.Diags);
  ErrorGatherer errorGatherer(Ctx.Diags);

  TypeChecker &TC = TypeChecker::createForContext(Ctx);

  TC.typeCheckExpression(parsedExpr, DC);

  if (parsedExpr) {
    ErrorFinder errorFinder;
    parsedExpr->walk(errorFinder);
    if (!errorFinder.hadError() && !errorGatherer.hadError()) {
      return true;
    }
  }

  return false;
}
