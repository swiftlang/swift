//===--- SyntaxParsingCache.cpp - Incremental syntax parsing lookup--------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/SyntaxParsingCache.h"

using namespace swift;
using namespace swift::syntax;

ArrayRef<TriviaPiece> getNextChildsLeadingTrivia(const Syntax *Node) {
  // Find the next parent with a successor child
  while (true) {
    auto Parent = Node->getParent();
    if (!Parent.hasValue()) {
      return {};
    }

    bool FoundSuccessor = false;
    for (size_t I = Node->getIndexInParent() + 1; I < Parent->getNumChildren();
         ++I) {
      if (Parent->getChild(I)) {
        Node = Parent->getChild(I).getPointer();
        FoundSuccessor = true;
        break;
      }
    }
    if (FoundSuccessor)
      break;

    // Continue traversing up the tree
    Node = Parent.getPointer();
  }
  // Find the first token of the successor node
  while (true) {
    bool LeafReached = true;
    for (size_t I = 0; I < Node->getNumChildren(); ++I) {
      if (Node->getChild(I)) {
        Node = Node->getChild(I).getPointer();
        LeafReached = false;
        break;
      }
    }
    if (LeafReached) {
      if (Node->isToken()) {
        return Node->getRaw()->getLeadingTrivia();
      } else {
        return {};
      }
    }
  }
}

bool SyntaxParsingCache::nodeCanBeReused(const Syntax &Node, size_t Position,
                                         SyntaxKind Kind) const {
  auto NodeStart = Node.getAbsolutePositionWithLeadingTrivia().getOffset();
  if (NodeStart != Position)
    return false;
  if (Node.getKind() != Kind)
    return false;

  // Calculate the leading trivia of the next node. Node can also not be reused
  // if an edit has been made in the next token's leading trivia
  auto NextNodeTrivia = getNextChildsLeadingTrivia(&Node);
  size_t TriviaLength = 0;
  for (auto TriviaPiece : NextNodeTrivia) {
    TriviaLength += TriviaPiece.getTextLength();
  }

  auto NodeEnd = NodeStart + Node.getTextLength();
  for (auto Edit : Edits) {
    // Check if this node or the trivia of the next node has been edited. If it
    // has, we cannot reuse it.
    if (Edit.intersectsOrTouchesRange(NodeStart, NodeEnd + TriviaLength)) {
      return false;
    }
  }

  // FIXME: Node can also not be reused if an edit has been made in the next
  // token's leading trivia

  return true;
}

llvm::Optional<Syntax> SyntaxParsingCache::lookUpFrom(const Syntax &Node,
                                                      size_t Position,
                                                      SyntaxKind Kind) {
  if (nodeCanBeReused(Node, Position, Kind)) {
    return Node;
  }

  for (size_t I = 0, E = Node.getNumChildren(); I < E; ++I) {
    llvm::Optional<Syntax> Child = Node.getChild(I);
    if (!Child.hasValue()) {
      continue;
    }
    auto ChildStart = Child->getAbsolutePositionWithLeadingTrivia().getOffset();
    auto ChildEnd = ChildStart + Child->getTextLength();
    if (ChildStart <= Position && Position < ChildEnd) {
      return lookUpFrom(Child.getValue(), Position, Kind);
    }
  }
  return llvm::None;
}

llvm::Optional<Syntax> SyntaxParsingCache::lookUp(size_t NewPosition,
                                                  SyntaxKind Kind) {
  // Undo the edits in reverse order
  size_t OldPosition = NewPosition;
  for (auto I = Edits.rbegin(), E = Edits.rend(); I != E; ++I) {
    auto Edit = *I;
    if (Edit.End <= OldPosition) {
      OldPosition =
          OldPosition - Edit.ReplacementLength + Edit.originalLength();
    }
  }

  auto Node = lookUpFrom(OldSyntaxTree, OldPosition, Kind);
  if (Node.hasValue()) {
    if (RecordReuseInformation) {
      ReusedRanges.push_back(
          {NewPosition, NewPosition + Node->getTextLength()});
    }
  }
  return Node;
}
