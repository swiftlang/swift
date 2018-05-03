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

llvm::Optional<Syntax> lookUpFrom(Syntax Node, size_t Position,
                                  SyntaxKind Kind) {
  if (Node.getAbsolutePosition().getOffset() == Position &&
      Node.getKind() == Kind) {
    return Node;
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
  // FIXME: Transform the new position into the position in the old file
  return lookUpFrom(OldSyntaxTree, NewPosition, Kind);
}
