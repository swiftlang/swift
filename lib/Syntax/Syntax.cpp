//===--- Syntax.cpp - Swift Syntax Implementation -------------------------===//
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

#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/SyntaxData.h"

using namespace swift;
using namespace swift::syntax;

RC<RawSyntax> Syntax::getRaw() const {
  return Data->getRaw();
}

SyntaxKind Syntax::getKind() const {
  return getRaw()->Kind;
}

void Syntax::print(llvm::raw_ostream &OS) const {
  getRaw()->print(OS);
}

void Syntax::dump() const {
  getRaw()->dump();
}

void Syntax::dump(llvm::raw_ostream &OS, unsigned Indent) const {
  getRaw()->dump(OS, 0);
}

bool Syntax::isType() const {
  return Data->isType();
}

bool Syntax::isDecl() const {
  return Data->isDecl();
}

bool Syntax::isStmt() const {
  return Data->isStmt();
}

bool Syntax::isExpr() const {
  return Data->isExpr();
}

bool Syntax::isPattern() const {
  return Data->isPattern();
}

bool Syntax::isUnknown() const {
  return Data->isUnknown();
}

bool Syntax::isPresent() const {
  return getRaw()->isPresent();
}

bool Syntax::isMissing() const {
  return getRaw()->isMissing();
}

llvm::Optional<Syntax> Syntax::getParent() const {
  auto ParentData = getData().Parent;
  if (ParentData == nullptr) return llvm::None;
  return llvm::Optional<Syntax> {
    Syntax { Root, ParentData }
  };
}

size_t Syntax::getNumChildren() const {
  size_t NonTokenChildren = 0;
  for (auto Child : getRaw()->Layout) {
    if (!Child->isToken()) {
      ++NonTokenChildren;
    }
  }
  return NonTokenChildren;
}

Syntax Syntax::getChild(const size_t N) const {
  // The actual index of the Nth non-token child.
  size_t ActualIndex = 0;
  // The number of non-token children we've seen.
  size_t NumNonTokenSeen = 0;
  for (auto Child : getRaw()->Layout) {
    // If we see a child that's not a token, count it.
    if (!Child->isToken()) {
      ++NumNonTokenSeen;
    }
    // If the number of children we've seen indexes the same (count - 1) as
    // the number we're looking for, then we're done.
    if (NumNonTokenSeen == N + 1) { break; }

    // Otherwise increment the actual index and keep searching.
    ++ActualIndex;
  }
  return Syntax { Root, Data->getChild(ActualIndex).get() };
}
