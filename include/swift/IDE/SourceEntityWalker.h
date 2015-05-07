//===- SourceEntityWalker.h - Routines for semantic source info -----------===//
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

#ifndef SWIFT_IDE_SEMANTIC_SOURCE_ENTITY_WALKER_H
#define SWIFT_IDE_SEMANTIC_SOURCE_ENTITY_WALKER_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/PointerUnion.h"
#include <string>

namespace clang {
  class Module;
}

namespace swift {
  class DeclContext;
  class Identifier;
  class SourceFile;
  class ModuleDecl;
  class ModuleEntity;
  class Decl;
  class ValueDecl;
  class TypeDecl;
  class Stmt;
  class Expr;

namespace ide {

/// An abstract class used to traverse the AST and provide source information.
/// Visitation happens in source-order and compiler-generated semantic info,
/// like implicit declarations, is ignored.
class SourceEntityWalker {
public:
  /// Walks the provided source file.
  /// \returns true if traversal was aborted, false otherwise.
  bool walk(SourceFile &SrcFile);
  /// Walks the provided module.
  /// \returns true if traversal was aborted, false otherwise.
  bool walk(ModuleDecl &Mod);
  /// Walks the provided Decl.
  /// \returns true if traversal was aborted, false otherwise.
  bool walk(Decl *D);
  /// Walks the provided DeclContext.
  /// \returns true if traversal was aborted, false otherwise.
  bool walk(DeclContext *DC);

  /// This method is called when first visiting a decl, before walking into its
  /// children.  If it returns false, the subtree is skipped.
  virtual bool walkToDeclPre(Decl *D, CharSourceRange Range) { return true; }

  /// This method is called after visiting the children of a decl. If it returns
  /// false, the remaining traversal is terminated and returns failure.
  virtual bool walkToDeclPost(Decl *D) { return true; }

  /// This method is called when first visiting a statement, before walking into
  /// its children.  If it returns false, the subtree is skipped.
  virtual bool walkToStmtPre(Stmt *S) { return true; }

  /// This method is called after visiting the children of a statement. If it
  /// returns false, the remaining traversal is terminated and returns failure.
  virtual bool walkToStmtPost(Stmt *S) { return true; }

  /// This method is called when first visiting an expression, before walking
  /// into its children.  If it returns false, the subtree is skipped.
  virtual bool walkToExprPre(Expr *E) { return true; }

  /// This method is called after visiting the children of an expression. If it
  /// returns false, the remaining traversal is terminated and returns failure.
  virtual bool walkToExprPost(Expr *E) { return true; }

  /// This method is called when a ValueDecl is referenced in source. If it
  /// returns false, the remaining traversal is terminated and returns failure.
  ///
  /// \param D the referenced decl.
  /// \param Range the source range of the source reference.
  /// \param CtorTyRef this is set when the entity is a reference to a
  /// \c ConstructorDecl, to point to the type declaration that the source
  /// refers to.
  virtual bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                                  TypeDecl *CtorTyRef) { return true; }

  /// This method is called for each keyword argument in a call expression.
  /// If it returns false, the remaining traversal is terminated and returns
  /// failure.
  ///
  /// \param Name the argument name.
  /// \param Range the source range of the argument name.
  /// \param D the referenced decl.
  virtual bool visitCallArgName(Identifier Name, CharSourceRange Range,
                                ValueDecl *D);

  /// This method is called when a Module is referenced in source.
  virtual bool visitModuleReference(ModuleEntity Mod, CharSourceRange Range);

protected:
  SourceEntityWalker() = default;
  SourceEntityWalker(const SourceEntityWalker &) = default;
  virtual ~SourceEntityWalker() {}

  virtual void anchor();
};

} // namespace ide
} // namespace swift

#endif
