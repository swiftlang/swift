//===--- HiddenLibSyntaxAction.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/HiddenLibSyntaxAction.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/Module.h"
#include "swift/Basic/OwnedString.h"
#include "swift/Parse/ParsedTrivia.h"
#include "swift/Parse/SyntaxParsingCache.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/SyntaxVisitor.h"
#include "swift/Syntax/Trivia.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;
using namespace swift::syntax;
using namespace llvm;

OpaqueSyntaxNode
HiddenLibSyntaxAction::makeHiddenNode(OpaqueSyntaxNode explicitActionNode,
                                      OpaqueSyntaxNode libSyntaxNode) {
  auto dat = NodeAllocator.Allocate();
  return new (dat) Node(explicitActionNode, libSyntaxNode);
}

OpaqueSyntaxNode HiddenLibSyntaxAction::recordToken(
    tok tokenKind, ArrayRef<ParsedTriviaPiece> leadingTrivia,
    ArrayRef<ParsedTriviaPiece> trailingTrivia, CharSourceRange range) {
  OpaqueSyntaxNode primaryNode = ExplicitAction->recordToken(
      tokenKind, leadingTrivia, trailingTrivia, range);
  OpaqueSyntaxNode secondaryNode = nullptr;

  if (areBothLibSyntax()) {
    secondaryNode = primaryNode;
  } else {
    secondaryNode = LibSyntaxAction->recordToken(tokenKind, leadingTrivia,
                                                 trailingTrivia, range);
  }

  return makeHiddenNode(primaryNode, secondaryNode);
}

OpaqueSyntaxNode HiddenLibSyntaxAction::recordMissingToken(tok tokenKind,
                                                           SourceLoc loc) {
  OpaqueSyntaxNode primaryNode =
      ExplicitAction->recordMissingToken(tokenKind, loc);
  OpaqueSyntaxNode secondaryNode = nullptr;

  if (areBothLibSyntax()) {
    secondaryNode = primaryNode;
  } else {
    secondaryNode = LibSyntaxAction->recordMissingToken(tokenKind, loc);
  }

  return makeHiddenNode(primaryNode, secondaryNode);
}

OpaqueSyntaxNode
HiddenLibSyntaxAction::recordRawSyntax(syntax::SyntaxKind kind,
                                       ArrayRef<OpaqueSyntaxNode> elements,
                                       CharSourceRange range) {
  OpaqueSyntaxNode primaryNode = nullptr;
  OpaqueSyntaxNode secondaryNode = nullptr;

  {
    SmallVector<OpaqueSyntaxNode, 4> primaryElements;
    primaryElements.reserve(elements.size());
    for (auto element : elements) {
      OpaqueSyntaxNode primaryElement = nullptr;
      if (element)
        primaryElement = ((Node *)element)->ExplicitActionNode;
      primaryElements.push_back(primaryElement);
    }

    primaryNode = ExplicitAction->recordRawSyntax(kind, primaryElements, range);
  }

  if (areBothLibSyntax()) {
    secondaryNode = primaryNode;
  } else {
    SmallVector<OpaqueSyntaxNode, 4> secondaryElements;
    secondaryElements.reserve(elements.size());
    for (auto element : elements) {
      OpaqueSyntaxNode secondaryElement = nullptr;
      if (element)
        secondaryElement = ((Node *)element)->LibSyntaxNode;
      secondaryElements.push_back(secondaryElement);
    }
    secondaryNode =
        LibSyntaxAction->recordRawSyntax(kind, secondaryElements, range);
  }

  return makeHiddenNode(primaryNode, secondaryNode);
}

std::pair<size_t, OpaqueSyntaxNode>
HiddenLibSyntaxAction::lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) {
  size_t length;
  OpaqueSyntaxNode n;
  std::tie(length, n) = ExplicitAction->lookupNode(lexerOffset, kind);
  if (length == 0)
    return {0, nullptr};
  return {length, makeHiddenNode(n, nullptr)};
}

RawSyntax *HiddenLibSyntaxAction::getLibSyntaxNodeFor(OpaqueSyntaxNode node) {
  auto hiddenNode = (Node *)node;
  return (RawSyntax *)hiddenNode->LibSyntaxNode;
}

OpaqueSyntaxNode
HiddenLibSyntaxAction::getExplicitNodeFor(OpaqueSyntaxNode node) {
  auto hiddenNode = (Node *)node;
  return hiddenNode->ExplicitActionNode;
}
