//===--- UnknownSyntax.cpp - Swift Unknown  Syntax Implementation ---------===//
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

#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/UnknownSyntax.h"

using namespace swift;
using namespace swift::syntax;

#pragma mark - unknown-syntax Data

UnknownSyntaxData::UnknownSyntaxData(const RC<RawSyntax> Raw,
                                     const SyntaxData *Parent,
                                     const CursorIndex IndexInParent)
    : SyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->isUnknown());
  for (auto RawChild : Raw->Layout) {
    if (!RawChild->isToken()) {
      CachedChildren.emplace_back(nullptr);
    }
  }
}

RC<UnknownSyntaxData> UnknownSyntaxData::make(RC<RawSyntax> Raw,
                                              const SyntaxData *Parent,
                                              CursorIndex IndexInParent) {

  auto UnknownRaw = RawSyntax::make(SyntaxKind::Unknown, Raw->Layout,
                                    Raw->Presence);

  return RC<UnknownSyntaxData> {
    new UnknownSyntaxData { UnknownRaw, Parent, IndexInParent }
  };
}

#pragma mark - unknown-syntax API

UnknownSyntax::UnknownSyntax(const RC<SyntaxData> Root,
                             const UnknownSyntaxData *Data)
  : Syntax(Root, Data) {}

size_t UnknownSyntax::getNumChildren() const {
  size_t Count = 0;
  for (auto Child : getRaw()->Layout) {
    if (Child->isToken()) {
      continue;
    }
    ++Count;
  }
  return Count;
}

Syntax UnknownSyntax::getChild(const size_t N) const {
  auto *MyData = getUnsafeData<UnknownSyntax>();

  if (auto RealizedChild = MyData->CachedChildren[N]) {
    return Syntax { Root, RealizedChild.get() };
  }

  assert(N < getNumChildren());
  assert(N < getRaw()->Layout.size());

  CursorIndex ChildLayoutIndex = 0;

  for (size_t LayoutIndex = 0, Left = N;
       LayoutIndex < getRaw()->Layout.size();
       ++LayoutIndex) {
    auto Child = getRaw()->Layout[LayoutIndex];
    if (Child->isToken()) {
      continue;
    }
    ChildLayoutIndex = LayoutIndex;
    if (Left == 0) {
      break;
    }
    --Left;
  }

  auto RawChild = getRaw()->Layout[ChildLayoutIndex];
  assert(RawChild->Kind != SyntaxKind::Token);

  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    MyData->CachedChildren.data() + N);

  SyntaxData::realizeSyntaxNode<Syntax>(ChildPtr, RawChild, MyData,
                                        ChildLayoutIndex);

  return Syntax { Root, MyData->CachedChildren[N].get() };
}

