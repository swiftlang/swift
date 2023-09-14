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

#include "AsyncRefactoring.h"

using namespace swift;
using namespace swift::refactoring::asyncrefactorings;

bool DeclReferenceFinder::walkToExprPre(Expr *E) {
  if (auto DRE = dyn_cast<DeclRefExpr>(E)) {
    if (DRE->getDecl() == Search) {
      HasFoundReference = true;
      return false;
    }
  }
  return true;
}

bool DeclReferenceFinder::containsReference(ASTNode Node,
                                            const ValueDecl *Search) {
  DeclReferenceFinder Checker(Search);
  Checker.walk(Node);
  return Checker.HasFoundReference;
}
