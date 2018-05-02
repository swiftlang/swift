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

llvm::Optional<Syntax> SyntaxParsingCache::lookUpFrom(Syntax Node,
                                                      size_t Position,
                                                      SyntaxKind Kind) const {
  if (Node.getAbsolutePosition().getOffset() == Position &&
      Node.getKind() == Kind) {
    // Check if this node has been edited. If it has, we cannot reuse it.
    bool NodeEdited = false;

    auto NodeStart = Node.getAbsolutePosition().getOffset();
    auto NodeEnd = NodeStart + Node.getTextLength();
    for (auto Edit : Edits) {
      if (Edit.intersectsOrTouchesRange(NodeStart, NodeEnd)) {
        NodeEdited = true;
      }
    }

    // FIXME: Node can also not be reused if an edit has been made in the next
    // token's leading trivia

    if (!NodeEdited) {
      return Node;
    }
  }

  for (size_t I = 0, E = Node.getNumChildren(); I < E; ++I) {
    llvm::Optional<Syntax> Child = Node.getChild(I);
    if (!Child.hasValue()) {
      continue;
    }
    auto ChildStart = Child->getAbsolutePosition().getOffset();
    auto ChildEnd = ChildStart + Child->getTextLength();
    if (ChildStart <= Position && Position < ChildEnd) {
      return lookUpFrom(Child.getValue(), Position, Kind);
    }
  }
  return llvm::None;
}

llvm::Optional<Syntax> SyntaxParsingCache::lookUp(size_t NewPosition,
                                                  SyntaxKind Kind) const {
  // Undo the edits in reverse order
  size_t OldPosition = NewPosition;
  for (auto I = Edits.rbegin(), E = Edits.rend(); I != E; ++I) {
    auto Edit = *I;
    if (Edit.End < OldPosition) {
      OldPosition =
          OldPosition - Edit.ReplacementLength + Edit.originalLength();
    }
  }

  return lookUpFrom(OldSyntaxTree, OldPosition, Kind);
}
