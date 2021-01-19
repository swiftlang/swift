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

Optional<AbsoluteRawSyntaxRef> AbsoluteRawSyntaxRef::getChildRef(
    AbsoluteSyntaxPosition::IndexInParentType Index) const {
  auto Raw = getRawRef();
  auto RawChildRef = Raw->getChildRef(Index);
  if (!RawChildRef) {
    return None;
  }

  AbsoluteSyntaxPosition Position = getPosition().advancedToFirstChild();
  SyntaxIdentifier NodeId = getNodeId().advancedToFirstChild();

  for (size_t I = 0; I < Index; ++I) {
    Position = Position.advancedBy(Raw->getChild(I));
    NodeId = NodeId.advancedBy(Raw->getChild(I));
  }

  AbsoluteSyntaxInfo Info(Position, NodeId);
  return AbsoluteRawSyntaxRef(RawChildRef, Info);
}

Optional<AbsoluteRawSyntaxRef> AbsoluteRawSyntaxRef::getFirstTokenRef() const {
  if (getRawRef()->isToken() && !getRawRef()->isMissing()) {
    return *this;
  }

  size_t NumChildren = getNumChildren();
  for (size_t I = 0; I < NumChildren; ++I) {
    if (auto Child = getChildRef(I)) {
      if (Child->getRawRef()->isMissing()) {
        continue;
      }

      if (auto Token = Child->getFirstTokenRef()) {
        return Token;
      }
    }
  }
  return None;
}

Optional<AbsoluteRawSyntaxRef> AbsoluteRawSyntaxRef::getLastTokenRef() const {
  if (getRawRef()->isToken() && !getRawRef()->isMissing()) {
    return *this;
  }

  size_t NumChildren = getNumChildren();
  if (NumChildren == 0) {
    return None;
  }
  for (int I = NumChildren - 1; I >= 0; --I) {
    if (auto Child = getChildRef(I)) {
      if (Child->getRawRef()->isMissing()) {
        continue;
      }

      if (auto Token = Child->getLastTokenRef()) {
        return Token;
      }
    }
  }
  return None;
}

Optional<AbsoluteRawSyntax> AbsoluteRawSyntax::getChild(
    AbsoluteSyntaxPosition::IndexInParentType Index) const {
  auto Raw = getRawRef();
  auto RawChild = Raw->getChild(Index);
  if (!RawChild) {
    return None;
  }

  AbsoluteSyntaxPosition Position = getPosition().advancedToFirstChild();
  SyntaxIdentifier NodeId = getNodeId().advancedToFirstChild();

  for (size_t I = 0; I < Index; ++I) {
    Position = Position.advancedBy(Raw->getChild(I));
    NodeId = NodeId.advancedBy(Raw->getChild(I));
  }

  AbsoluteSyntaxInfo Info(Position, NodeId);
  return AbsoluteRawSyntax(RawChild, Info);
}

raw_ostream &llvm::operator<<(raw_ostream &OS,
                              swift::syntax::AbsoluteOffsetPosition Pos) {
  OS << "Offset " << Pos.getOffset();
  return OS;
}
