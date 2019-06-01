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

OpaqueSyntaxNode HiddenLibSyntaxAction::recordToken(
    tok tokenKind, ArrayRef<ParsedTriviaPiece> leadingTrivia,
    ArrayRef<ParsedTriviaPiece> trailingTrivia, CharSourceRange range) {
  OpaqueSyntaxNode primaryNode = ExplicitAction->recordToken(
      tokenKind, leadingTrivia, trailingTrivia, range);

  if (!areBothLibSyntax()) {
    OpaqueSyntaxNode secondaryNode = LibSyntaxAction->recordToken(
        tokenKind, leadingTrivia, trailingTrivia, range);
    OpaqueNodeMap[primaryNode] = secondaryNode;
  }

  return primaryNode;
}

OpaqueSyntaxNode HiddenLibSyntaxAction::recordMissingToken(tok tokenKind,
                                                           SourceLoc loc) {
  OpaqueSyntaxNode primaryNode =
      ExplicitAction->recordMissingToken(tokenKind, loc);

  if (!areBothLibSyntax()) {
    OpaqueSyntaxNode secondaryNode =
        LibSyntaxAction->recordMissingToken(tokenKind, loc);
    OpaqueNodeMap[primaryNode] = secondaryNode;
  }

  return primaryNode;
}

OpaqueSyntaxNode
HiddenLibSyntaxAction::recordRawSyntax(syntax::SyntaxKind kind,
                                       ArrayRef<OpaqueSyntaxNode> elements,
                                       CharSourceRange range) {
  OpaqueSyntaxNode primaryNode =
      ExplicitAction->recordRawSyntax(kind, elements, range);

  if (!areBothLibSyntax()) {
    SmallVector<OpaqueSyntaxNode, 4> secondaryElements;
    secondaryElements.reserve(elements.size());
    for (auto &&element : elements) {
      secondaryElements.push_back(OpaqueNodeMap[element]);
    }
    OpaqueSyntaxNode secondaryNode =
        LibSyntaxAction->recordRawSyntax(kind, secondaryElements, range);
    OpaqueNodeMap[primaryNode] = secondaryNode;
  }

  return primaryNode;
}

std::pair<size_t, OpaqueSyntaxNode>
HiddenLibSyntaxAction::lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) {
  return ExplicitAction->lookupNode(lexerOffset, kind);
}

OpaqueSyntaxNode
HiddenLibSyntaxAction::getLibSyntaxNodeFor(OpaqueSyntaxNode explicitNode) {
  if (!areBothLibSyntax())
    return OpaqueNodeMap[explicitNode];

  return explicitNode;
}
