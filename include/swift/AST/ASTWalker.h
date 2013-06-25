//===--- ASTWalker.h - Class for walking the AST ----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ASTWALKER_H
#define SWIFT_AST_ASTWALKER_H

#include "llvm/ADT/PointerUnion.h"
#include <utility>

namespace swift {

class Decl;
class Expr;
class Stmt;
class Pattern;
  
/// \brief An abstract class used to traverse an AST.
class ASTWalker {
public:
  /// \brief The parent of the node we are visiting.
  llvm::PointerUnion3<Expr *, Stmt *, Pattern *> Parent;

  /// This method is called when first visiting an expression
  /// before walking into its children.
  ///
  /// \param E The expression to check.
  ///
  /// \returns a pair indicating whether to visit the children along with
  /// the expression that should replace this expression in the tree. If the
  /// latter is null, the traversal will be terminated.
  ///
  /// The default implementation returns \c {true, E}.
  virtual std::pair<bool, Expr *> walkToExprPre(Expr *E) {
    return { true, E };
  }

  /// This method is called after visiting an expression's children.
  /// If it returns null, the walk is terminated; otherwise, the
  /// returned expression is spliced in where the old expression
  /// previously appeared.
  ///
  /// The default implementation always returns its argument.
  virtual Expr *walkToExprPost(Expr *E) { return E; }

  /// This method is called when first visiting a statement before
  /// walking into its children.
  ///
  /// \param S The statement to check.
  ///
  /// \returns a pair indicating whether to visit the children along with
  /// the statement that should replace this statement in the tree. If the
  /// latter is null, the traversal will be terminated.
  ///
  /// The default implementation returns \c {true, S}.
  virtual std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) {
    return { true, S };
  }

  /// This method is called after visiting a statement's children.  If
  /// it returns null, the walk is terminated; otherwise, the returned
  /// statement is spliced in where the old statement previously
  /// appeared.
  ///
  /// The default implementation always returns its argument.
  virtual Stmt *walkToStmtPost(Stmt *S) { return S; }

  /// This method is called when first visiting a pattern before walking into
  /// its children.
  ///
  /// \param P The statement to check.
  ///
  /// \returns a pair indicating whether to visit the children along with
  /// the statement that should replace this statement in the tree. If the
  /// latter is null, the traversal will be terminated.
  ///
  /// The default implementation returns \c {true, P}.
  virtual std::pair<bool, Pattern*> walkToPatternPre(Pattern *P) {
    return { true, P };
  }

  /// This method is called after visiting a pattern's children.  If
  /// it returns null, the walk is terminated; otherwise, the returned
  /// pattern is spliced in where the old statement previously
  /// appeared.
  ///
  /// The default implementation always returns its argument.
  virtual Pattern *walkToPatternPost(Pattern *P) { return P; }

  /// walkToDeclPre - This method is called when first visiting a decl, before
  /// walking into its children.  If it returns false, the subtree is skipped.
  ///
  /// \param D The declaration to check. The callee may update this declaration
  /// in-place.
  virtual bool walkToDeclPre(Decl *D) { return true; }

  /// walkToDeclPost - This method is called after visiting the children of a
  /// decl.  If it returns false, the remaining traversal is terminated and
  /// returns failure.
  virtual bool walkToDeclPost(Decl *D) { return true; }

protected:
  ASTWalker() = default;
  ASTWalker(const ASTWalker &) = default;
  virtual ~ASTWalker() {}
  
  virtual void anchor();
};

} // end namespace swift

#endif
