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

#ifndef SWIFT_REFACTORING_CONTEXTFINDER_H
#define SWIFT_REFACTORING_CONTEXTFINDER_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/SourceManager.h"
#include "swift/IDE/SourceEntityWalker.h"

namespace swift {
namespace refactoring {

class ContextFinder : public SourceEntityWalker {
  SourceFile &SF;
  ASTContext &Ctx;
  SourceManager &SM;
  SourceRange Target;
  std::function<bool(ASTNode)> IsContext;
  SmallVector<ASTNode, 4> AllContexts;
  bool contains(ASTNode Enclosing) {
    auto Result = SM.rangeContainsRespectingReplacedRanges(
        Enclosing.getSourceRange(), Target);
    if (Result && IsContext(Enclosing)) {
      AllContexts.push_back(Enclosing);
    }
    return Result;
  }

public:
  ContextFinder(
      SourceFile &SF, ASTNode TargetNode,
      std::function<bool(ASTNode)> IsContext = [](ASTNode N) { return true; })
      : SF(SF), Ctx(SF.getASTContext()), SM(Ctx.SourceMgr),
        Target(TargetNode.getSourceRange()), IsContext(IsContext) {}

  ContextFinder(
      SourceFile &SF, SourceLoc TargetLoc,
      std::function<bool(ASTNode)> IsContext = [](ASTNode N) { return true; })
      : SF(SF), Ctx(SF.getASTContext()), SM(Ctx.SourceMgr), Target(TargetLoc),
        IsContext(IsContext) {
    assert(TargetLoc.isValid() && "Invalid loc to find");
  }

  // Only need expansions for the expands refactoring, but we
  // skip nodes that don't contain the passed location anyway.
  virtual MacroWalking getMacroWalkingBehavior() const override {
    return MacroWalking::ArgumentsAndExpansion;
  }

  bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
    return contains(D);
  }
  bool walkToStmtPre(Stmt *S) override { return contains(S); }
  bool walkToExprPre(Expr *E) override { return contains(E); }
  void resolve() { walk(SF); }
  ArrayRef<ASTNode> getContexts() const { return llvm::ArrayRef(AllContexts); }
};

} // namespace refactoring
} // namespace swift

#endif
