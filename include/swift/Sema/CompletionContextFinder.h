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

namespace constraints {
class SyntacticElementTarget;
}

class CompletionContextFinder : public ASTWalker {
  /// If we are completing inside an expression, the \c CodeCompletionExpr that
  /// represents the code completion token.

  /// The AST node that represents the code completion token, either as a
  /// \c CodeCompletionExpr or a \c KeyPathExpr which contains a code completion
  /// component.
  llvm::PointerUnion<CodeCompletionExpr *, const KeyPathExpr *> CompletionNode;

public:
  MacroWalking getMacroWalkingBehavior() const override {
    return MacroWalking::Arguments;
  }

  /// Finder for completion contexts within the provided SyntacticElementTarget.
  CompletionContextFinder(constraints::SyntacticElementTarget target);

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override;

  PreWalkAction walkToDeclPre(Decl *D) override;

  bool hasCompletionExpr() const {
    return CompletionNode.dyn_cast<CodeCompletionExpr *>() != nullptr;
  }

  CodeCompletionExpr *getCompletionExpr() const {
    assert(hasCompletionExpr());
    return cast<CodeCompletionExpr *>(CompletionNode);
  }

  bool hasCompletionKeyPathComponent() const {
    return CompletionNode.dyn_cast<const KeyPathExpr *>() != nullptr;
  }

  bool hasCompletion() const {
    return !CompletionNode.isNull();
  }

  /// If we are completing in a key path, returns the \c KeyPath that contains
  /// the code completion component.
  const KeyPathExpr *getKeyPathContainingCompletionComponent() const {
    assert(hasCompletionKeyPathComponent());
    return cast<const KeyPathExpr *>(CompletionNode);
  }

  /// If we are completing in a key path, returns the index at which the key
  /// path has the code completion component.
  size_t getKeyPathCompletionComponentIndex() const;
};

/// Returns \c true if \p range is valid and contains the IDE inspection
/// target. This performs the underlying check based on \c CharSourceRange
/// to make sure we correctly return \c true if the ide inspection target
/// is inside a string literal that's the last token in \p range.
bool containsIDEInspectionTarget(SourceRange range,
                                 const SourceManager &SourceMgr);

} // end namespace swift

#endif // SWIFT_SEMA_COMPLETIONCONTEXTFINDER_H
