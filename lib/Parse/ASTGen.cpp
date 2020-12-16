//===--- ASTGen.cpp -------------------------------------------------------===//
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

#include "swift/Parse/ASTGen.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/SourceManager.h"

using namespace swift;
using namespace swift::syntax;

StringRef ASTGen::copyAndStripUnderscores(StringRef Orig, ASTContext &Context) {
  char *start = static_cast<char *>(Context.Allocate(Orig.size(), 1));
  char *p = start;

  if (p) {
    for (char c : Orig) {
      if (c != '_') {
        *p++ = c;
      }
    }
  }

  return StringRef(start, p - start);
}

//===----------------------------------------------------------------------===//
// MARK: - Other

StringRef ASTGen::copyAndStripUnderscores(StringRef Orig) {
  return copyAndStripUnderscores(Orig, Context);
}

SourceLoc ASTGen::advanceLocBegin(const SourceLoc &Loc, const Syntax &Node) {
  return Loc.getAdvancedLoc(Node.getAbsolutePosition().getOffset());
}

SourceLoc ASTGen::advanceLocEnd(const SourceLoc &Loc,
                                const TokenSyntax &Token) {
  return advanceLocAfter(Loc, Token.withTrailingTrivia({}));
}

SourceLoc ASTGen::advanceLocAfter(const SourceLoc &Loc, const Syntax &Node) {
  return Loc.getAdvancedLoc(
      Node.getAbsoluteEndPositionAfterTrailingTrivia().getOffset());
}
