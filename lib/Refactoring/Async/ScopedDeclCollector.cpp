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

const ScopedDeclCollector::RefDeclsTy *
ScopedDeclCollector::getReferencedDecls(Stmt *Scope) const {
  auto Res = ReferencedDecls.find(Scope);
  if (Res == ReferencedDecls.end())
    return nullptr;
  return &Res->second;
}

bool ScopedDeclCollector::walkToDeclPre(Decl *D, CharSourceRange Range) {
  if (ScopeStack.empty() || D->isImplicit())
    return true;

  ScopeStack.back().DeclaredDecls.insert(D);
  if (isa<DeclContext>(D))
    (*ScopeStack.back().ReferencedDecls)[D] += 1;
  return true;
}

bool ScopedDeclCollector::walkToExprPre(Expr *E) {
  if (ScopeStack.empty())
    return true;

  if (!E->isImplicit()) {
    if (auto *DRE = dyn_cast<DeclRefExpr>(E)) {
      if (auto *D = DRE->getDecl()) {
        // If we have a reference that isn't declared in the same scope,
        // increment the number of references to that decl.
        if (!D->isImplicit() && !ScopeStack.back().DeclaredDecls.count(D)) {
          (*ScopeStack.back().ReferencedDecls)[D] += 1;

          // Also add the async alternative of a function to prevent
          // collisions if a call is replaced with the alternative.
          if (auto *AFD = dyn_cast<AbstractFunctionDecl>(D)) {
            if (auto *Alternative = AFD->getAsyncAlternative())
              (*ScopeStack.back().ReferencedDecls)[Alternative] += 1;
          }
        }
      }
    }
  }
  return true;
}

bool ScopedDeclCollector::walkToStmtPre(Stmt *S) {
  // Purposely check \c BraceStmt here rather than \c startsNewScope.
  // References in the condition should be applied to the previous scope, not
  // the scope of that statement.
  if (isa<BraceStmt>(S))
    ScopeStack.emplace_back(&ReferencedDecls[S]);
  return true;
}

bool ScopedDeclCollector::walkToStmtPost(Stmt *S) {
  if (isa<BraceStmt>(S)) {
    size_t NumScopes = ScopeStack.size();
    if (NumScopes >= 2) {
      // Add any referenced decls to the parent scope that weren't declared
      // there.
      auto &ParentStack = ScopeStack[NumScopes - 2];
      for (auto DeclAndNumRefs : *ScopeStack.back().ReferencedDecls) {
        auto *D = DeclAndNumRefs.first;
        if (!ParentStack.DeclaredDecls.count(D))
          (*ParentStack.ReferencedDecls)[D] += DeclAndNumRefs.second;
      }
    }
    ScopeStack.pop_back();
  }
  return true;
}
