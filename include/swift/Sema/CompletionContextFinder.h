//===--- CompletionContextFinder.h ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_COMPLETIONCONTEXTFINDER_H
#define SWIFT_SEMA_COMPLETIONCONTEXTFINDER_H

#include "swift/AST/ASTNode.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Expr.h"
#include "swift/IDE/TypeCheckCompletionCallback.h"

namespace swift {

class CompletionContextFinder : public ASTWalker {
  enum class ContextKind {
    FallbackExpression,
    StringInterpolation,
    SingleStmtClosure,
    MultiStmtClosure,
    ErrorExpression
  };

  struct Context {
    ContextKind Kind;
    Expr *E;
  };

  /// Stack of all "interesting" contexts up to code completion expression.
  llvm::SmallVector<Context, 4> Contexts;

  /// If we are completing inside an expression, the \c CodeCompletionExpr that
  /// represents the code completion token.

  /// The AST node that represents the code completion token, either as a
  /// \c CodeCompletionExpr or a \c KeyPathExpr which contains a code completion
  /// component.
  llvm::PointerUnion<CodeCompletionExpr *, const KeyPathExpr *> CompletionNode;

  Expr *InitialExpr = nullptr;
  DeclContext *InitialDC;

public:
  /// Finder for completion contexts within the provided initial expression.
  CompletionContextFinder(ASTNode initialNode, DeclContext *DC)
      : InitialExpr(initialNode.dyn_cast<Expr *>()), InitialDC(DC) {
    assert(DC);
    initialNode.walk(*this);
  };

  /// Finder for completion contexts within the outermost non-closure context of
  /// the code completion expression's direct context.
  CompletionContextFinder(DeclContext *completionDC) : InitialDC(completionDC) {
    while (auto *ACE = dyn_cast<AbstractClosureExpr>(InitialDC))
      InitialDC = ACE->getParent();
    InitialDC->walkContext(*this);
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override;

  PostWalkResult<Expr *> walkToExprPost(Expr *E) override;

  /// Check whether code completion expression is located inside of a
  /// multi-statement closure.
  bool locatedInMultiStmtClosure() const {
    return hasContext(ContextKind::MultiStmtClosure);
  }

  bool locatedInStringInterpolation() const {
    return hasContext(ContextKind::StringInterpolation);
  }

  bool hasCompletionExpr() const {
    return CompletionNode.dyn_cast<CodeCompletionExpr *>() != nullptr;
  }

  CodeCompletionExpr *getCompletionExpr() const {
    assert(hasCompletionExpr());
    return CompletionNode.get<CodeCompletionExpr *>();
  }

  bool hasCompletionKeyPathComponent() const {
    return CompletionNode.dyn_cast<const KeyPathExpr *>() != nullptr;
  }

  /// If we are completing in a key path, returns the \c KeyPath that contains
  /// the code completion component.
  const KeyPathExpr *getKeyPathContainingCompletionComponent() const {
    assert(hasCompletionKeyPathComponent());
    return CompletionNode.get<const KeyPathExpr *>();
  }

  /// If we are completing in a key path, returns the index at which the key
  /// path has the code completion component.
  size_t getKeyPathCompletionComponentIndex() const;

  struct Fallback {
    Expr *E;               ///< The fallback expression.
    DeclContext *DC;       ///< The fallback expression's decl context.
    bool SeparatePrecheck; ///< True if the fallback may require prechecking.
  };

  /// As a fallback sometimes its useful to not only type-check
  /// code completion expression directly but instead add some
  /// of the enclosing context e.g. when completion is an argument
  /// to a call.
  Optional<Fallback> getFallbackCompletionExpr() const;

private:
  bool hasContext(ContextKind kind) const {
    return llvm::find_if(Contexts, [&kind](const Context &currContext) {
             return currContext.Kind == kind;
           }) != Contexts.end();
  }
};

} // end namespace swift

#endif // SWIFT_SEMA_COMPLETIONCONTEXTFINDER_H
