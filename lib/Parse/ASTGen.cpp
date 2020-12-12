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
// MARK: - Private

StringRef ASTGen::copyAndStripUnderscores(StringRef Orig) {
  return copyAndStripUnderscores(Orig, Context);
}

SourceLoc ASTGen::advanceLocBegin(const SourceLoc &Loc, const Syntax &Node) {
  return Loc.getAdvancedLoc(Node.getAbsolutePosition().getOffset());
}

SourceLoc ASTGen::advanceLocEnd(const SourceLoc &Loc, const Syntax &Node) {
  if (!Node.isMissing()) {
    // NOTE: We cannot use 'getLastToken()' because it doesn't take string
    // literal expressions into account.
    if (Node.isToken() || Node.is<StringLiteralExprSyntax>())
      return advanceLocBegin(Loc, Node);
    for (size_t I = Node.getNumChildren(); I != 0; --I)
      if (auto Child = Node.getChild(I - 1))
        return advanceLocEnd(Loc, *Child);
  }
  if (auto Prev = Node.getPreviousNode())
    return advanceLocEnd(Loc, *Prev);
  assert(false && "No tokens in tree?");
  return Loc;
}

Expr *ASTGen::generateMagicIdentifierLiteralExpr(const TokenSyntax &PoundToken,
                                                 const SourceLoc &Loc) {
  auto Kind = getMagicIdentifierLiteralKind(PoundToken.getTokenKind());
  SourceLoc TokenLoc = advanceLocBegin(Loc, PoundToken);
  return new (Context) MagicIdentifierLiteralExpr(Kind, TokenLoc);
}

/// Map magic literal tokens such as #file to their
/// MagicIdentifierLiteralExpr kind.
MagicIdentifierLiteralExpr::Kind
ASTGen::getMagicIdentifierLiteralKind(tok Kind) {
  switch (Kind) {
  case tok::pound_file:
    // TODO: Enable by default at the next source break. (SR-13199)
    return Context.LangOpts.EnableConcisePoundFile
               ? MagicIdentifierLiteralExpr::FileIDSpelledAsFile
               : MagicIdentifierLiteralExpr::FilePathSpelledAsFile;
#define MAGIC_IDENTIFIER_TOKEN(NAME, TOKEN)                                    \
  case tok::TOKEN:                                                             \
    return MagicIdentifierLiteralExpr::Kind::NAME;
#include "swift/AST/MagicIdentifierKinds.def"
  default:
    llvm_unreachable("not a magic literal");
  }
}
