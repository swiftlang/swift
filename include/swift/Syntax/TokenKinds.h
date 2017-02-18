//===--- TokenKinds.h - Token Kinds Interface -------------------*- C++ -*-===//
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
//
//  This file defines the Token kinds.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TOKENKINDS_H
#define SWIFT_TOKENKINDS_H

namespace swift {
enum class tok {
  unknown = 0,
  eof,
  code_complete,
  identifier,
  oper_binary_unspaced,   // "x+y"
  oper_binary_spaced,     // "x + y"
  oper_postfix,
  oper_prefix,
  dollarident,
  integer_literal,
  floating_literal,
  string_literal,
  sil_local_name,      // %42 in SIL mode.
  comment,

#define KEYWORD(X) kw_ ## X,
#define PUNCTUATOR(X, Y) X,
#define POUND_KEYWORD(X) pound_ ## X,
#include "swift/Syntax/TokenKinds.def"

  NUM_TOKENS
};
} // end namespace swift

#endif // SWIFT_TOKENKINDS_H

