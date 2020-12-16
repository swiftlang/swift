//===--- ASTGenExpr.cpp ---------------------------------------------------===//
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

Expr *ASTGen::generate(const IntegerLiteralExprSyntax &Expr, const SourceLoc &Loc) {
  TokenSyntax Digits = Expr.getDigits();
  StringRef Text = copyAndStripUnderscores(Digits.getText());
  auto DigitsLoc = advanceLocBegin(Loc, Digits);
  return new (Context) IntegerLiteralExpr(Text, DigitsLoc);
}

//===----------------------------------------------------------------------===//
// MARK: - Private

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
