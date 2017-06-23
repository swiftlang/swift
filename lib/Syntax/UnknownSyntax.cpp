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

void UnknownSyntax::validate() const {
  assert(Data->Raw->isUnknown());
}

#pragma mark - unknown-syntax API

size_t UnknownSyntax::getNumChildren() const {
  size_t NonTokenChildren = 0;
  for (auto Child : getRaw()->Layout) {
    if (!Child->isToken()) {
      ++NonTokenChildren;
    }
  }
  return NonTokenChildren;
}

Syntax UnknownSyntax::getChild(const size_t N) const {
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

