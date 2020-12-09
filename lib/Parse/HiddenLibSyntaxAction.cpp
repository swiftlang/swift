//===--- HiddenLibSyntaxAction.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/HiddenLibSyntaxAction.h"

#include "swift/AST/ASTContext.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "swift/Syntax/Trivia.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;
using namespace swift::syntax;
using namespace llvm;

OpaqueSyntaxNode
HiddenLibSyntaxAction::makeHiddenNode(OpaqueSyntaxNode explicitActionNode,
                                      OpaqueSyntaxNode libSyntaxNode) {
  auto dat = NodeAllocator.Allocate();
  return new (dat) HiddenNode(explicitActionNode, libSyntaxNode);
}

OpaqueSyntaxNode HiddenLibSyntaxAction::recordToken(
    tok tokenKind, ArrayRef<ParsedTriviaPiece> leadingTrivia,
    ArrayRef<ParsedTriviaPiece> trailingTrivia, CharSourceRange range) {

  OpaqueSyntaxNode explicitActionNode;
  if (ExplicitAction) {
    explicitActionNode = ExplicitAction->recordToken(
      tokenKind, leadingTrivia, trailingTrivia, range);
  } else {
    explicitActionNode = nullptr;
  }

  OpaqueSyntaxNode libSyntaxActionNode;
  if (ExplicitAction == LibSyntaxAction) {
    libSyntaxActionNode = explicitActionNode;
  } else {
    libSyntaxActionNode = LibSyntaxAction->recordToken(tokenKind, leadingTrivia,
                                                       trailingTrivia, range);
  }

  return makeHiddenNode(explicitActionNode, libSyntaxActionNode);
}

OpaqueSyntaxNode HiddenLibSyntaxAction::recordMissingToken(tok tokenKind,
                                                           SourceLoc loc) {
  OpaqueSyntaxNode explicitActionNode;
  if (ExplicitAction) {
    explicitActionNode =
        ExplicitAction->recordMissingToken(tokenKind, loc);
  } else {
    explicitActionNode = nullptr;
  }

  OpaqueSyntaxNode libSyntaxActionNode;
  if (ExplicitAction == LibSyntaxAction) {
    libSyntaxActionNode = explicitActionNode;
  } else {
    libSyntaxActionNode = LibSyntaxAction->recordMissingToken(tokenKind, loc);
  }

  return makeHiddenNode(explicitActionNode, libSyntaxActionNode);
}

OpaqueSyntaxNode
HiddenLibSyntaxAction::recordRawSyntax(syntax::SyntaxKind kind,
                                       ArrayRef<OpaqueSyntaxNode> elements,
                                       CharSourceRange range) {
  OpaqueSyntaxNode explicitActionNode;
  if (ExplicitAction) {
    SmallVector<OpaqueSyntaxNode, 4> explicitActionElements;
    explicitActionElements.reserve(elements.size());
    for (auto element : elements) {
      OpaqueSyntaxNode explicitActionElement = nullptr;
      if (element) {
        explicitActionElement =
            static_cast<HiddenNode *>(element)->ExplicitActionNode;
      }
      explicitActionElements.push_back(explicitActionElement);
    }

    explicitActionNode =
        ExplicitAction->recordRawSyntax(kind, explicitActionElements, range);
  } else {
    explicitActionNode = nullptr;
  }

  OpaqueSyntaxNode libSyntaxActionNode;
  if (ExplicitAction == LibSyntaxAction) {
    libSyntaxActionNode = explicitActionNode;
  } else {
    SmallVector<OpaqueSyntaxNode, 4> libSyntaxActionElements;
    libSyntaxActionElements.reserve(elements.size());
    for (auto element : elements) {
      OpaqueSyntaxNode libSyntaxActionElement = nullptr;
      if (element) {
        libSyntaxActionElement =
            static_cast<HiddenNode *>(element)->LibSyntaxNode;
      }
      libSyntaxActionElements.push_back(libSyntaxActionElement);
    }
    libSyntaxActionNode =
        LibSyntaxAction->recordRawSyntax(kind, libSyntaxActionElements, range);
  }

  return makeHiddenNode(explicitActionNode, libSyntaxActionNode);
}

Optional<syntax::SourceFileSyntax>
HiddenLibSyntaxAction::realizeSyntaxRoot(OpaqueSyntaxNode root,
                                         const SourceFile &SF) {
  auto node = static_cast<HiddenNode *>(root);
  return LibSyntaxAction->realizeSyntaxRoot(node->LibSyntaxNode, SF);
}

void HiddenLibSyntaxAction::discardRecordedNode(OpaqueSyntaxNode opaqueN) {
  if (!opaqueN) {
    return;
  }
  auto node = static_cast<HiddenNode *>(opaqueN);
  if (ExplicitAction) {
    ExplicitAction->discardRecordedNode(node->ExplicitActionNode);
  }
  if (ExplicitAction != LibSyntaxAction) {
    LibSyntaxAction->discardRecordedNode(node->LibSyntaxNode);
  }
}

std::pair<size_t, OpaqueSyntaxNode>
HiddenLibSyntaxAction::lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) {
  if (ExplicitAction) {
    // TODO: (syntax-parse) We only perform the node lookup in the explicit action
    // If HiddenSyntaxAction should stay, we should find an implementation that
    // also performs lookup in the LibSyntaxAction
    size_t length;
    OpaqueSyntaxNode n;
    std::tie(length, n) = ExplicitAction->lookupNode(lexerOffset, kind);
    if (length == 0) {
      return {0, nullptr};
    }
    return {length, makeHiddenNode(n, nullptr)};
  } else {
    return {0, nullptr};

  }
}

RawSyntax *HiddenLibSyntaxAction::getLibSyntaxNodeFor(OpaqueSyntaxNode node) {
  auto hiddenNode = static_cast<HiddenNode *>(node);
  return static_cast<RawSyntax *>(hiddenNode->LibSyntaxNode);
}

OpaqueSyntaxNode
HiddenLibSyntaxAction::getExplicitNodeFor(OpaqueSyntaxNode node) {
  return static_cast<HiddenNode *>(node)->ExplicitActionNode;
}
