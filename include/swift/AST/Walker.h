//===--- Walker.h - Class for walking the AST -------------------*- C++ -*-===//
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

#ifndef SWIFT_AST_WALKER_H
#define SWIFT_AST_WALKER_H

#include "llvm/ADT/PointerUnion.h"

namespace swift {

class Expr;
class Stmt;
  
/// \brief An abstract class used to traverse an AST.
class Walker {
public:
  /// \brief The parent of the node we are visiting.
  llvm::PointerUnion<Expr *, Stmt *> Parent;

  /// This method is called when first visiting an expression,
  /// before walking into its children.  If it returns false, the
  /// subtree is ignored.
  ///
  /// The default implementation always returns true.
  virtual bool walkToExprPre(Expr *E);

  /// This method is called after visiting an expression's children.
  /// If it returns null, the walk is terminated; otherwise, the
  /// returned expression is spliced in where the old expression
  /// previously appeared.
  ///
  /// The default implementation always returns its argument.
  virtual Expr *walkToExprPost(Expr *E);

  /// This method is called when first visiting a statement, before
  /// walking into its children.  If it returns false, the subtree is
  /// ignored.
  ///
  /// The default implementation always returns true.
  virtual bool walkToStmtPre(Stmt *S);

  /// This method is called after visiting a statement's children.  If
  /// it returns null, the walk is terminated; otherwise, the returned
  /// statement is spliced in where the old statement previously
  /// appeared.
  ///
  /// The default implementation always returns its argument.
  virtual Stmt *walkToStmtPost(Stmt *S);

protected:
  Walker() = default;
  Walker(const Walker &walker) = default;
  ~Walker() = default;
};

} // end namespace swift

#endif
