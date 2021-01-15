//===--- AbsoluteRawSyntax.cpp ----------------------------------*- C++ -*-===//
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

#include "swift/Syntax/AbsoluteRawSyntax.h"

using namespace swift;
using namespace swift::syntax;

std::atomic<SyntaxIdentifier::RootIdType> SyntaxIdentifier::NextUnusedRootId(0);

SyntaxIndexInTree
SyntaxIndexInTree::advancedBy(const RC<RawSyntax> &Raw) const {
  auto NewIndexInTree = IndexInTree;
  if (Raw) {
    NewIndexInTree += Raw->getTotalNodes();
  }
  return SyntaxIndexInTree(NewIndexInTree);
}

SyntaxIndexInTree
SyntaxIndexInTree::reversedBy(const RC<RawSyntax> &Raw) const {
  auto NewIndexInTree = IndexInTree;
  if (Raw) {
    NewIndexInTree -= Raw->getTotalNodes();
  }
  return SyntaxIndexInTree(NewIndexInTree);
}

SyntaxIndexInTree SyntaxIndexInTree::advancedToFirstChild() const {
  auto NewIndexInTree = IndexInTree + 1;
  return SyntaxIndexInTree(NewIndexInTree);
}

AbsoluteSyntaxPosition
AbsoluteSyntaxPosition::advancedBy(const RC<RawSyntax> &Raw) const {
  OffsetType NewOffset = Offset;
  if (Raw) {
    NewOffset += Raw->getTextLength();
  }
  IndexInParentType NewIndexInParent = IndexInParent + 1;
  return AbsoluteSyntaxPosition(NewOffset, NewIndexInParent);
}

AbsoluteSyntaxPosition
AbsoluteSyntaxPosition::reversedBy(const RC<RawSyntax> &Raw) const {
  OffsetType NewOffset = Offset;
  if (Raw) {
    NewOffset -= Raw->getTextLength();
  }
  IndexInParentType NewIndexInParent = IndexInParent - 1;
  return AbsoluteSyntaxPosition(NewOffset, NewIndexInParent);
}

raw_ostream &llvm::operator<<(raw_ostream &OS,
                              swift::syntax::AbsoluteOffsetPosition Pos) {
  OS << "Offset " << Pos.getOffset();
  return OS;
}
