//===--- HiddenLibSyntaxAction.h ------------------------------------------===//
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

#ifndef SWIFT_PARSE_HIDDENLIBSYNTAXACTION_H
#define SWIFT_PARSE_HIDDENLIBSYNTAXACTION_H

#include "swift/Parse/SyntaxParseActions.h"
#include "swift/SyntaxParse/SyntaxTreeCreator.h"
#include "llvm/Support/Allocator.h"

namespace swift {
namespace syntax {
class RawSyntax;
}

/// Holds an explicitly provided action and uses it to handle all function
/// calls. Also hides an implicit SyntaxTreeCreator and ensures libSyntax nodes
/// are always created. Provides an interface to map results of the explicitly
/// provided action to the hidden libSyntax action.
// todo [gsoc]: remove when possible
class HiddenLibSyntaxAction : public SyntaxParseActions {

  struct Node {
    OpaqueSyntaxNode ExplicitActionNode;
    OpaqueSyntaxNode LibSyntaxNode;

    Node(OpaqueSyntaxNode ExplicitActionNode, OpaqueSyntaxNode LibSyntaxNode)
        : ExplicitActionNode(ExplicitActionNode), LibSyntaxNode(LibSyntaxNode) {
    }
  };

  std::shared_ptr<SyntaxParseActions> ExplicitAction;
  std::shared_ptr<SyntaxTreeCreator> LibSyntaxAction;
  llvm::SpecificBumpPtrAllocator<Node> NodeAllocator;

  bool areBothLibSyntax() {
    return ExplicitAction->getOpaqueKind() == OpaqueSyntaxNodeKind::LibSyntax;
  }

  OpaqueSyntaxNode makeHiddenNode(OpaqueSyntaxNode explicitActionNode,
                                  OpaqueSyntaxNode libSyntaxNode);

public:
  HiddenLibSyntaxAction(
      const std::shared_ptr<SyntaxParseActions> &SPActions,
      const std::shared_ptr<SyntaxTreeCreator> &LibSyntaxAction)
      : ExplicitAction(SPActions != nullptr ? SPActions : LibSyntaxAction),
        LibSyntaxAction(LibSyntaxAction){};

  OpaqueSyntaxNode recordToken(tok tokenKind,
                               ArrayRef<ParsedTriviaPiece> leadingTrivia,
                               ArrayRef<ParsedTriviaPiece> trailingTrivia,
                               CharSourceRange range) override;

  OpaqueSyntaxNode recordMissingToken(tok tokenKind, SourceLoc loc) override;

  OpaqueSyntaxNode recordRawSyntax(syntax::SyntaxKind kind,
                                   ArrayRef<OpaqueSyntaxNode> elements,
                                   CharSourceRange range) override;

  std::pair<size_t, OpaqueSyntaxNode>
  lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) override;

  void discardRecordedNode(OpaqueSyntaxNode node) override;

  OpaqueSyntaxNodeKind getOpaqueKind() override {
    return ExplicitAction->getOpaqueKind();
  }

  /// Returns the libSyntax node from the specified node that has been created
  /// by this action.
  syntax::RawSyntax *getLibSyntaxNodeFor(OpaqueSyntaxNode node);

  /// Returns the node created by explicit syntax action from the specified
  /// node that has been created by this action.
  OpaqueSyntaxNode getExplicitNodeFor(OpaqueSyntaxNode node);

  bool isReleaseNeeded() {
    return ExplicitAction == LibSyntaxAction || !areBothLibSyntax();
  }

  /// Returns the underlying libSyntax SyntaxTreeCreator.
  std::shared_ptr<SyntaxTreeCreator> getLibSyntaxAction() {
    return LibSyntaxAction;
  }
};
} // namespace swift

#endif // SWIFT_PARSE_HIDDENLIBSYNTAXACTION_H
