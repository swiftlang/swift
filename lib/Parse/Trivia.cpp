//===--- Lexer.h - Swift Source Trivia --------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Lexer.h"
#include "swift/Syntax/Trivia.h"

using namespace swift;
using namespace syntax;

bool Trivia::trailingWhitespaceContainsNewline() const {
  if (Text.empty())
    return false;

  auto Str = Text.str();
  auto RBegin = Str.end() - 1;
  auto REnd = Str.begin() - 1;

  for (auto it = RBegin; it != REnd; --it) {
    if (isNewline(*it))
      return true;
    if (syntax::isWhitespace(*it))
      continue;
    else
      return false;
  }
  return false;
}

