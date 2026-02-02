//===--- CompletionContextFinder.cpp --------------------------------------===//
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

#include "swift/Sema/CompletionContextFinder.h"
#include "swift/Basic/Assertions.h"
#include "swift/Parse/Lexer.h"
#include "swift/Sema/SyntacticElementTarget.h"

using namespace swift;
using namespace constraints;

CompletionContextFinder::CompletionContextFinder(
    SyntacticElementTarget target) {
  target.walk(*this);
}

ASTWalker::PreWalkResult<Expr *>
CompletionContextFinder::walkToExprPre(Expr *E) {
  if (auto *CCE = dyn_cast<CodeCompletionExpr>(E)) {
    CompletionNode = CCE;
    return Action::Stop();
  }
  if (auto *KeyPath = dyn_cast<KeyPathExpr>(E)) {
    for (auto &component : KeyPath->getComponents()) {
      if (component.getKind() == KeyPathExpr::Component::Kind::CodeCompletion) {
        CompletionNode = KeyPath;
        return Action::Stop();
      }
    }
    // Code completion in key paths is modelled by a code completion component
    // Don't walk the key path's parsed expressions.
    return Action::SkipNode(E);
  }

  return Action::Continue(E);
}

ASTWalker::PreWalkAction CompletionContextFinder::walkToDeclPre(Decl *D) {
  // Otherwise, follow the same rule as the ConstraintSystem, where only
  // nested PatternBindingDecls are solved as part of the system. Local decls
  // are handled by TypeCheckASTNodeAtLocRequest.
  return Action::VisitNodeIf(isa<PatternBindingDecl>(D));
}

size_t CompletionContextFinder::getKeyPathCompletionComponentIndex() const {
  size_t ComponentIndex = 0;
  auto Components = getKeyPathContainingCompletionComponent()->getComponents();
  for (auto &Component : Components) {
    if (Component.getKind() == KeyPathExpr::Component::Kind::CodeCompletion) {
      break;
    } else {
      ComponentIndex++;
    }
  }
  assert(ComponentIndex < Components.size() &&
         "No completion component in the key path?");
  return ComponentIndex;
}

bool swift::containsIDEInspectionTarget(SourceRange range,
                                        const SourceManager &SourceMgr) {
  if (range.isInvalid())
    return false;
  auto charRange = Lexer::getCharSourceRangeFromSourceRange(SourceMgr, range);
  return SourceMgr.rangeContainsIDEInspectionTarget(charRange);
}
