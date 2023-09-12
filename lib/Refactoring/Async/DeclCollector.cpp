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
#include "Utils.h"

using namespace swift;
using namespace swift::refactoring::asyncrefactorings;

void DeclCollector::collect(BraceStmt *Scope, SourceFile &SF,
                            llvm::DenseSet<const Decl *> &Decls) {
  DeclCollector Collector(Decls);
  if (Scope) {
    for (auto Node : Scope->getElements()) {
      Collector.walk(Node);
    }
  } else {
    Collector.walk(SF);
  }
}

bool DeclCollector::walkToDeclPre(Decl *D, CharSourceRange Range) {
  // Want to walk through top level code decls (which are implicitly added
  // for top level non-decl code) and pattern binding decls (which contain
  // the var decls that we care about).
  if (isa<TopLevelCodeDecl>(D) || isa<PatternBindingDecl>(D))
    return true;

  if (!D->isImplicit())
    Decls.insert(D);
  return false;
}

bool DeclCollector::walkToExprPre(Expr *E) { return !isa<ClosureExpr>(E); }

bool DeclCollector::walkToStmtPre(Stmt *S) {
  return S->isImplicit() || !startsNewScope(S);
}
