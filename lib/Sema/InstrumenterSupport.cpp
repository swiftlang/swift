//===--- InstrumenterSupport.h - Instrumenter Support ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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

using namespace swift;
using namespace swift::instrumenter_support;

void InstrumenterBase::anchor() {}

bool InstrumenterBase::doTypeCheckImpl(ASTContext &Ctx, DeclContext *DC,
                                       Expr * &parsedExpr) {
  DiagnosticEngine diags(Ctx.SourceMgr);
  ErrorGatherer errorGatherer(diags);

  TypeChecker TC(Ctx, diags);

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
