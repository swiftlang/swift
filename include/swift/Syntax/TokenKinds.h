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
#define TOKEN(X) X,
#include "swift/Syntax/TokenKinds.def"

  NUM_TOKENS
};

/// Check whether a token kind is known to have any specific text content.
/// e.g., tol::l_paren has determined text however tok::identifier doesn't.
bool isTokenTextDetermined(tok kind);

/// If a token kind has determined text, return the text; otherwise assert.
StringRef getTokenText(tok kind);

/// True if the token is any keyword.
bool isTokenKeyword(tok kind);

void dumpTokenKind(llvm::raw_ostream &os, tok kind);
} // end namespace swift

#endif // SWIFT_TOKENKINDS_H

