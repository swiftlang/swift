//===--- SourceEntityWalker.h - Routines for semantic source info ---------===//
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

#ifndef SWIFT_IDE_SOURCE_ENTITY_WALKER_H
#define SWIFT_IDE_SOURCE_ENTITY_WALKER_H

#include "swift/AST/ASTWalker.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/SaveAndRestore.h"

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
  class ExtensionDecl;
  class Stmt;
  class Expr;
  class Type;
  struct ASTNode;

/// An abstract class used to traverse the AST and provide source information.
/// Visitation happens in source-order and compiler-generated semantic info,
/// like implicit declarations, is ignored.
///
/// The \c walkTo*Pre methods return an \c ASTWalker::PreWalkAction:
/// - \c Action::Continue() visits the children, then calls \c walkTo*Post.
/// - \c Action::SkipChildren() skips the children but still calls
///   \c walkTo*Post for the current node.
/// - \c Action::SkipNode() skips the children and does not call
///   \c walkTo*Post for the current node.
/// - \c Action::Stop() terminates the traversal entirely. No further
///   \c walk* calls are issued; nodes that have already received a
///   \c walkTo*Pre call will *not* receive a \c walkTo*Post call.
///
/// The \c walkTo*Post methods return an \c ASTWalker::PostWalkAction:
/// - \c Action::Continue() resumes the traversal.
/// - \c Action::Stop() terminates the traversal.
///
/// The \c visit* event callbacks similarly return an
/// \c ASTWalker::PostWalkAction (\c Continue or \c Stop).
class SourceEntityWalker {
public:
  // Convenience to use ASTWalker's action types without full qualification.
  using Action = ASTWalker::Action;
  using PreWalkAction = ASTWalker::PreWalkAction;
  using PostWalkAction = ASTWalker::PostWalkAction;

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
  /// Walks the provided Stmt.
  /// \returns true if traversal was aborted, false otherwise.
  bool walk(Stmt *S);
  /// Walks the provided Expr.
  /// \returns true if traversal was aborted, false otherwise.
  bool walk(Expr *E);
  /// Walks the provided Pattern.
  /// \returns true if traversal was aborted, false otherwise.
  bool walk(Pattern *P);
  /// Walks the provided ASTNode.
  /// \returns true if traversal was aborted, false otherwise.
  bool walk(ASTNode N);

  /// This method is called when first visiting a decl, before walking into
  /// its children.
  virtual PreWalkAction walkToDeclPre(Decl *D, CharSourceRange Range) {
    return Action::Continue();
  }

  /// This method is called after visiting the children of a decl.
  virtual PostWalkAction walkToDeclPost(Decl *D) {
    return Action::Continue();
  }

  /// This method is called in AST order, unlike \c walkToDecl* which are
  /// called in source order. It is guaranteed to be called in balance with
  /// its counterpart \c endBalancedASTOrderDeclVisit.
  virtual void beginBalancedASTOrderDeclVisit(Decl *D) {};

  /// This method is called after declaration visitation in AST order.
  virtual void endBalancedASTOrderDeclVisit(Decl *D) {};

  /// This method is called when first visiting a statement, before walking
  /// into its children.
  virtual PreWalkAction walkToStmtPre(Stmt *S) { return Action::Continue(); }

  /// This method is called after visiting the children of a statement.
  virtual PostWalkAction walkToStmtPost(Stmt *S) {
    return Action::Continue();
  }

  /// This method is called when first visiting an expression, before walking
  /// into its children.
  virtual PreWalkAction walkToExprPre(Expr *E) { return Action::Continue(); }

  /// This method is called after visiting the children of an expression.
  virtual PostWalkAction walkToExprPost(Expr *E) {
    return Action::Continue();
  }

  /// This method is called when first visiting a pattern, before walking
  /// into its children.
  virtual PreWalkAction walkToPatternPre(Pattern *P) {
    return Action::Continue();
  }

  /// This method is called after visiting the children of a pattern.
  virtual PostWalkAction walkToPatternPost(Pattern *P) {
    return Action::Continue();
  }

  /// This method is called when first visiting a type representation, before
  /// walking into its children.
  virtual PreWalkAction walkToTypeReprPre(TypeRepr *T) {
    return Action::Continue();
  }

  /// This method is called after visiting the children of a type
  /// representation.
  virtual PostWalkAction walkToTypeReprPost(TypeRepr *T) {
    return Action::Continue();
  }

  /// This method is called when a ValueDecl is referenced in source.
  ///
  /// \param D the referenced decl.
  /// \param Range the source range of the source reference.
  /// \param CtorTyRef this is set when the entity is a reference to a
  /// \c ConstructorDecl, to point to the type declaration that the source
  /// refers to.
  /// \param ExtTyRef this is set when the entity is a reference to a type loc
  /// in \c ExtensionDecl.
  virtual PostWalkAction visitDeclReference(ValueDecl *D, SourceRange Range,
                                            TypeDecl *CtorTyRef,
                                            ExtensionDecl *ExtTyRef, Type T,
                                            ReferenceMetaData Data);

  /// This method is called when a ValueDecl for a subscript is referenced in
  /// source.
  ///
  /// Unlike the ordinary decl reference (\c visitDeclReference), this method
  /// is called twice for each subscript reference: on open and close
  /// brackets.
  ///
  /// \param D the referenced decl.
  /// \param Range the source range of the source reference.
  /// \param Data whether this is a read, write or read/write access, etc.
  /// \param IsOpenBracket this is \c true when the method is called on an
  /// open bracket.
  virtual PostWalkAction visitSubscriptReference(ValueDecl *D,
                                                  SourceRange Range,
                                                  ReferenceMetaData Data,
                                                  bool IsOpenBracket);

  /// This method is called when a ValueDecl for a callAsFunction decl is
  /// referenced in source.
  ///
  /// \param D the referenced decl.
  /// \param Range the source range of the source reference.
  /// \param Data whether this is a read, write or read/write access, etc.
  virtual PostWalkAction visitCallAsFunctionReference(ValueDecl *D,
                                                      SourceRange Range,
                                                      ReferenceMetaData Data);

  /// This method is called for each keyword argument in a call expression.
  ///
  /// \param Name the argument name.
  /// \param Range the source range of the argument name.
  /// \param D the referenced decl.
  virtual PostWalkAction
  visitCallArgName(Identifier Name, CharSourceRange Range, ValueDecl *D);

  /// This method is called for each external argument name in function-like
  /// declarations like constructor, function and subscript. The function is
  /// called only when an external argument label is specifically specified,
  /// like func foo(External Internal: Int) {}.
  ///
  /// \param Name the argument name.
  /// \param StartLoc the source loc of the argument name start.
  /// \param D the function-like decl.
  virtual PostWalkAction visitDeclarationArgumentName(Identifier Name,
                                                      SourceLoc StartLoc,
                                                      ValueDecl *D);

  /// This method is called when a Module is referenced in source.
  virtual PostWalkAction visitModuleReference(ModuleEntity Mod,
                                              CharSourceRange Range);

  virtual bool shouldWalkIntoGenericParams() { return true; }

  virtual bool shouldWalkIntoForEachDesugaredStmt() { return false; }

  /// Only walk the arguments of a macro, to represent the source as written.
  virtual MacroWalking getMacroWalkingBehavior() const {
    return MacroWalking::Arguments;
  }

protected:
  SourceEntityWalker() = default;
  SourceEntityWalker(const SourceEntityWalker &) = default;
  virtual ~SourceEntityWalker() {}

  virtual void anchor();

  /// Retrieve the current ASTWalker being used to traverse the AST.
  const ASTWalker &getWalker() const {
    assert(Walker && "Not walking!");
    return *Walker;
  }

private:
  ASTWalker *Walker = nullptr;

  /// Utility that lets us keep track of an ASTWalker when walking.
  bool performWalk(ASTWalker &W, llvm::function_ref<bool(void)> DoWalk) {
    llvm::SaveAndRestore<ASTWalker *> SV(Walker, &W);
    return DoWalk();
  }
};

} // namespace swift

#endif
