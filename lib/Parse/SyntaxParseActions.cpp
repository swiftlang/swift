//===--- SyntaxParseActions.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/SyntaxParseActions.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/SyntaxKind.h"

using namespace swift;

struct DeferredTokenNode {
  bool IsMissing;
  tok TokenKind;
  StringRef LeadingTrivia;
  StringRef TrailingTrivia;
  /// The range of the token including trivia.
  CharSourceRange Range;

  DeferredTokenNode(bool IsMissing, tok TokenKind, StringRef LeadingTrivia,
                    StringRef TrailingTrivia, CharSourceRange Range)
      : IsMissing(IsMissing), TokenKind(TokenKind),
        LeadingTrivia(LeadingTrivia), TrailingTrivia(TrailingTrivia),
        Range(Range) {}
};

struct DeferredLayoutNode {
  syntax::SyntaxKind Kind;
  ArrayRef<RecordedOrDeferredNode> Children;
  unsigned Length;

  DeferredLayoutNode(syntax::SyntaxKind Kind,
                     ArrayRef<RecordedOrDeferredNode> Children, unsigned Length)
      : Kind(Kind), Children(Children), Length(Length) {}
};

OpaqueSyntaxNode SyntaxParseActions::makeDeferredToken(tok tokenKind,
                                                       StringRef leadingTrivia,
                                                       StringRef trailingTrivia,
                                                       CharSourceRange range,
                                                       bool isMissing) {
  return new (DeferredNodeAllocator) DeferredTokenNode(
      isMissing, tokenKind, leadingTrivia, trailingTrivia, range);
}

OpaqueSyntaxNode SyntaxParseActions::makeDeferredLayout(
    syntax::SyntaxKind k, bool isMissing,
    const ArrayRef<RecordedOrDeferredNode> &children) {
  assert(!isMissing && "Missing layout nodes not implemented yet");

  // Compute the length of this node.
  unsigned length = 0;
  for (auto &child : children) {
    switch (child.getKind()) {
    case RecordedOrDeferredNode::Kind::Null:
      break;
    case RecordedOrDeferredNode::Kind::Recorded:
      llvm_unreachable("Children of deferred nodes must also be deferred");
      break;
    case RecordedOrDeferredNode::Kind::DeferredLayout:
      length +=
          static_cast<const DeferredLayoutNode *>(child.getOpaque())->Length;
      break;
    case RecordedOrDeferredNode::Kind::DeferredToken:
      length += static_cast<const DeferredTokenNode *>(child.getOpaque())
                    ->Range.getByteLength();
      break;
    }
  }

  return new (DeferredNodeAllocator) DeferredLayoutNode(k, children, length);
}

OpaqueSyntaxNode
SyntaxParseActions::recordDeferredToken(OpaqueSyntaxNode deferred) {
  auto Data = static_cast<const DeferredTokenNode *>(deferred);
  if (Data->IsMissing) {
    return recordMissingToken(Data->TokenKind, Data->Range.getStart());
  } else {
    return recordToken(Data->TokenKind, Data->LeadingTrivia,
                       Data->TrailingTrivia, Data->Range);
  }
}

OpaqueSyntaxNode
SyntaxParseActions::recordDeferredLayout(OpaqueSyntaxNode deferred) {
  auto Data = static_cast<const DeferredLayoutNode *>(deferred);

  auto childrenStore =
      DeferredNodeAllocator.Allocate<OpaqueSyntaxNode>(Data->Children.size());
  MutableArrayRef<OpaqueSyntaxNode> children =
      llvm::makeMutableArrayRef(childrenStore, Data->Children.size());

  for (size_t i = 0; i < Data->Children.size(); ++i) {
    auto Child = Data->Children[i];
    switch (Child.getKind()) {
    case RecordedOrDeferredNode::Kind::Null:
      children[i] = Child.getOpaque();
      break;
    case RecordedOrDeferredNode::Kind::Recorded:
      llvm_unreachable("Children of deferred nodes must also be deferred");
      break;
    case RecordedOrDeferredNode::Kind::DeferredLayout:
      children[i] = recordDeferredLayout(Child.getOpaque());
      break;
    case RecordedOrDeferredNode::Kind::DeferredToken:
      children[i] = recordDeferredToken(Child.getOpaque());
      break;
    }
  }
  return recordRawSyntax(Data->Kind, children);
}

DeferredNodeInfo SyntaxParseActions::getDeferredChild(OpaqueSyntaxNode node,
                                                      size_t ChildIndex,
                                                      SourceLoc StartLoc) {
  auto Data = static_cast<const DeferredLayoutNode *>(node);

  // Compute the start offset of the child node by advancing StartLoc by the
  // length of all previous child nodes.
  for (unsigned i = 0; i < ChildIndex; ++i) {
    auto Child = Data->Children[i];
    switch (Child.getKind()) {
    case RecordedOrDeferredNode::Kind::Null:
      break;
    case RecordedOrDeferredNode::Kind::Recorded:
      llvm_unreachable("Children of deferred nodes must also be deferred");
    case RecordedOrDeferredNode::Kind::DeferredLayout:
      StartLoc = StartLoc.getAdvancedLoc(
          static_cast<const DeferredLayoutNode *>(Child.getOpaque())->Length);
      break;
    case RecordedOrDeferredNode::Kind::DeferredToken:
      StartLoc = StartLoc.getAdvancedLoc(
          static_cast<const DeferredTokenNode *>(Child.getOpaque())
              ->Range.getByteLength());
      break;
    }
  }

  auto Child = Data->Children[ChildIndex];
  switch (Child.getKind()) {
  case RecordedOrDeferredNode::Kind::Null:
    llvm_unreachable("A child node should not be Null");
  case RecordedOrDeferredNode::Kind::Recorded:
    llvm_unreachable("Children of deferred nodes must also be deferred");
    break;
  case RecordedOrDeferredNode::Kind::DeferredLayout: {
    auto ChildData = static_cast<const DeferredLayoutNode *>(Child.getOpaque());
    return DeferredNodeInfo(ChildData, ChildData->Kind, tok::NUM_TOKENS,
                            /*IsMissing=*/false,
                            CharSourceRange(StartLoc, ChildData->Length));
  }
  case RecordedOrDeferredNode::Kind::DeferredToken: {
    auto ChildData = static_cast<const DeferredTokenNode *>(Child.getOpaque());
    return DeferredNodeInfo(ChildData, syntax::SyntaxKind::Token,
                            ChildData->TokenKind, ChildData->IsMissing,
                            ChildData->Range);
  }
  }
}
