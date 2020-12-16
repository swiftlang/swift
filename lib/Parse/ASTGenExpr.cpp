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

Expr *ASTGen::generate(const BooleanLiteralExprSyntax &Expr, const SourceLoc &Loc) {
  TokenSyntax Literal = Expr.getBooleanLiteral();
  assert(Literal.getTokenKind() == tok::kw_true || 
         Literal.getTokenKind() == tok::kw_false);
  bool Value = Literal.getTokenKind() == tok::kw_true;
  SourceLoc LiteralLoc = advanceLocBegin(Loc, Literal);
  return new (Context) BooleanLiteralExpr(Value, LiteralLoc);
}

Expr *ASTGen::generate(const FloatLiteralExprSyntax &Expr, const SourceLoc &Loc) {
  TokenSyntax Digits = Expr.getFloatingDigits();
  StringRef Text = copyAndStripUnderscores(Digits.getText());
  auto DigitsLoc = advanceLocBegin(Loc, Digits);
  return new (Context) FloatLiteralExpr(Text, DigitsLoc);
}

Expr *ASTGen::generate(const IntegerLiteralExprSyntax &Expr, const SourceLoc &Loc) {
  TokenSyntax Digits = Expr.getDigits();
  StringRef Text = copyAndStripUnderscores(Digits.getText());
  auto DigitsLoc = advanceLocBegin(Loc, Digits);
  return new (Context) IntegerLiteralExpr(Text, DigitsLoc);
}

Expr *ASTGen::generate(const NilLiteralExprSyntax &Expr, const SourceLoc &Loc) {
  TokenSyntax Nil = Expr.getNilKeyword();
  SourceLoc NilLoc = advanceLocBegin(Loc, Nil);
  return new (Context) NilLiteralExpr(NilLoc);
}

Expr *ASTGen::generate(const PoundColumnExprSyntax &Expr, const SourceLoc &Loc) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundColumn(), Loc);
}

Expr *ASTGen::generate(const PoundDsohandleExprSyntax &Expr, const SourceLoc &Loc) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundDsohandle(), Loc);
}

Expr *ASTGen::generate(const PoundFileExprSyntax &Expr, const SourceLoc &Loc) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundFile(), Loc);
}

Expr *ASTGen::generate(const PoundFileIDExprSyntax &Expr, const SourceLoc &Loc) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundFileID(), Loc);
}

Expr *ASTGen::generate(const PoundFilePathExprSyntax &Expr, const SourceLoc &Loc) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundFilePath(), Loc);
}

Expr *ASTGen::generate(const PoundFunctionExprSyntax &Expr, const SourceLoc &Loc) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundFunction(), Loc);
}

Expr *ASTGen::generate(const PoundLineExprSyntax &Expr, const SourceLoc &Loc) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundLine(), Loc);
}

Expr *ASTGen::generate(const UnknownExprSyntax &Expr, const SourceLoc &Loc) {
  if (Expr.getNumChildren() == 1 && Expr.getChild(0)->isToken()) {
    Syntax Token = *Expr.getChild(0);
    tok Kind = Token.getRaw()->getTokenKind();
    switch (Kind) {
    case tok::kw___FILE__:
    case tok::kw___LINE__:
    case tok::kw___COLUMN__:
    case tok::kw___FUNCTION__:
    case tok::kw___DSO_HANDLE__: {
      auto MagicKind = getMagicIdentifierLiteralKind(Kind);
      SourceLoc TokenLoc = advanceLocBegin(Loc, Token);
      return new (Context) MagicIdentifierLiteralExpr(MagicKind, TokenLoc);
    }
    default:
      return nullptr;
    }
  }
  return nullptr;
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
