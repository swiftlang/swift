//===--- ASTNode.cpp - Swift Language ASTs --------------------------------===//
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
// This file implements the ASTNode, which is a union of Stmt, Expr, and Decl.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTNode.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/Basic/SourceLoc.h"

using namespace swift;

SourceRange ASTNode::getSourceRange() const {
  if (const Expr *E = this->dyn_cast<Expr*>())
    return E->getSourceRange();
  if (const Stmt *S = this->dyn_cast<Stmt*>())
    return S->getSourceRange();
  if (const Decl *D = this->dyn_cast<Decl*>())
    return D->getSourceRange();
  llvm_unreachable("unsupported AST node");
}

/// \brief Return the location of the start of the statement.
SourceLoc ASTNode::getStartLoc() const {
  return getSourceRange().Start;
}

/// \brief Return the location of the end of the statement.
SourceLoc ASTNode::getEndLoc() const {
  return getSourceRange().End;
}

DeclContext *ASTNode::getAsDeclContext() const {
  if (Expr *E = this->dyn_cast<Expr*>()) {
    if (isa<AbstractClosureExpr>(E))
      return static_cast<AbstractClosureExpr*>(E);
  } else if (is<Stmt*>()) {
    return nullptr;
  } else if (Decl *D = this->dyn_cast<Decl*>()) {
    if (isa<DeclContext>(D))
      return cast<DeclContext>(D);
  } else if (getOpaqueValue())
    llvm_unreachable("unsupported AST node");
  return nullptr;
}

void ASTNode::walk(ASTWalker &Walker) {
  if (Expr *E = this->dyn_cast<Expr*>())
    E->walk(Walker);
  else if (Stmt *S = this->dyn_cast<Stmt*>())
    S->walk(Walker);
  else if (Decl *D = this->dyn_cast<Decl*>())
    D->walk(Walker);
  else
    llvm_unreachable("unsupported AST node");
}
void ASTNode::walk(SourceEntityWalker &Walker) {
  if (Expr *E = this->dyn_cast<Expr*>())
    Walker.walk(E);
  else if (Stmt *S = this->dyn_cast<Stmt*>())
    Walker.walk(S);
  else if (Decl *D = this->dyn_cast<Decl*>())
    Walker.walk(D);
  else
    llvm_unreachable("unsupported AST node");
}

#define FUNC(T)                                                               \
bool ASTNode::is##T(T##Kind Kind) const {                                     \
  if (!is<T*>())                                                              \
    return false;                                                             \
  return get<T*>()->getKind() == Kind;                                        \
}
FUNC(Stmt)
FUNC(Expr)
FUNC(Decl)
#undef FUNC
