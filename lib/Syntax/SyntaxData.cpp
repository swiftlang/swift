//===--- SyntaxData.cpp - Swift Syntax Data Implementation ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Syntax/SyntaxData.h"

using namespace swift;
using namespace swift::syntax;

// MARK: - SyntaxDataRef

void SyntaxDataRef::dump(llvm::raw_ostream &OS) const {
  getRaw()->dump(OS, 0);
  OS << '\n';
}

void SyntaxDataRef::dump() const { dump(llvm::errs()); }

// MARK: - SyntaxData

RC<const SyntaxData>
SyntaxData::getChild(AbsoluteSyntaxPosition::IndexInParentType Index) const {
  auto AbsoluteRaw = getAbsoluteRaw().getChild(Index);
  if (AbsoluteRaw) {
    return RC<SyntaxData>(
        new SyntaxData(*AbsoluteRaw, /*Parent=*/RC<const SyntaxData>(this)));
  } else {
    return nullptr;
  }
}

RC<const SyntaxData> SyntaxData::getPreviousNode() const {
  if (size_t N = getIndexInParent()) {
    if (hasParent()) {
      for (size_t I = N - 1; ; --I) {
        if (auto C = getParent()->getChild(I)) {
          if (C->getRaw()->isPresent() && C->getFirstToken())
            return C;
        }
        if (I == 0)
          break;
      }
    }
  }
  return hasParent() ? getParent()->getPreviousNode() : nullptr;
}

RC<const SyntaxData> SyntaxData::getNextNode() const {
  if (hasParent()) {
    for (size_t I = getIndexInParent() + 1, N = getParent()->getNumChildren();
         I != N; ++I) {
      if (auto C = getParent()->getChild(I)) {
        if (C->getRaw()->isPresent() && C->getFirstToken())
          return C;
      }
    }
    return getParent()->getNextNode();
  }
  return nullptr;
}

RC<const SyntaxData> SyntaxData::getFirstToken() const {
  /// getFirstToken and getLastToken cannot be implemented on SyntaxDataRef
  /// because we might need to traverse through multiple nodes to reach the
  /// first token. When returning this token, the parent nodes are being
  /// discarded and thus its parent pointer would point to invalid memory.
  if (getRaw()->isToken() && !getRaw()->isMissing()) {
    return RC<const SyntaxData>(this);
  }

  for (size_t I = 0, E = getNumChildren(); I < E; ++I) {
    if (auto Child = getChild(I)) {
      if (Child->getRaw()->isMissing()) {
        continue;
      }

      if (auto Token = Child->getFirstToken()) {
        return Token;
      }
    }
  }
  return nullptr;
}

RC<const SyntaxData> SyntaxData::getLastToken() const {
  // Also see comment in getFirstToken.
  if (getRaw()->isToken() && !getRaw()->isMissing()) {
    return RC<const SyntaxData>(this);
  }

  if (getNumChildren() == 0) {
    return nullptr;
  }
  for (int I = getNumChildren() - 1; I >= 0; --I) {
    if (auto Child = getChild(I)) {
      if (Child->getRaw()->isMissing()) {
        continue;
      }

      if (auto Token = Child->getLastToken()) {
        return Token;
      }
    }
  }
  return nullptr;
}

RC<const SyntaxData> SyntaxData::replacingSelf(const RawSyntax *NewRaw) const {
  if (hasParent()) {
    auto NewParent = getParent()->replacingChild(NewRaw, getIndexInParent());
    auto NewSelf = AbsoluteRaw.replacingSelf(
        NewRaw, NewParent->AbsoluteRaw.getNodeId().getRootId());
    return RC<const SyntaxData>(new SyntaxData(NewSelf, NewParent));
  } else {
    auto NewSelf = AbsoluteRawSyntax::forRoot(NewRaw);
    return RC<const SyntaxData>(new SyntaxData(NewSelf));
  }
}
