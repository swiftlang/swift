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

BooleanLiteralExpr *ASTGen::generate(BooleanLiteralExprSyntax &Expr) {
  TokenSyntax Literal = Expr.getBooleanLiteral();
  assert(Literal.getTokenKind() == tok::kw_true || 
         Literal.getTokenKind() == tok::kw_false);
  bool Value = Literal.getTokenKind() == tok::kw_true;
  SourceLoc Loc = topLoc();
  return new (Context) BooleanLiteralExpr(Value, Loc);
}

FloatLiteralExpr *ASTGen::generate(FloatLiteralExprSyntax &Expr) {
  TokenSyntax FloatingDigits = Expr.getFloatingDigits();
  StringRef Text = copyAndStripUnderscores(FloatingDigits.getText());
  SourceLoc Loc = topLoc();
  return new (Context) FloatLiteralExpr(Text, Loc);
}

IntegerLiteralExpr *ASTGen::generate(IntegerLiteralExprSyntax &Expr) {
  TokenSyntax Digits = Expr.getDigits();
  StringRef Text = copyAndStripUnderscores(Digits.getText());
  SourceLoc Loc = topLoc();
  return new (Context) IntegerLiteralExpr(Text, Loc);
}

NilLiteralExpr *ASTGen::generate(NilLiteralExprSyntax &Expr) {
  TokenSyntax Nil = Expr.getNilKeyword();
  SourceLoc Loc = topLoc();
  return new (Context) NilLiteralExpr(Loc);
}

MagicIdentifierLiteralExpr *ASTGen::generate(PoundColumnExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundColumn());
}

MagicIdentifierLiteralExpr *ASTGen::generate(PoundDsohandleExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundDsohandle());
}

MagicIdentifierLiteralExpr *ASTGen::generate(PoundFileExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundFile());
}

MagicIdentifierLiteralExpr *ASTGen::generate(PoundFileIDExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundFileID());
}

MagicIdentifierLiteralExpr *ASTGen::generate(PoundFilePathExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundFilePath());
}

MagicIdentifierLiteralExpr *ASTGen::generate(PoundLineExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundLine());
}

MagicIdentifierLiteralExpr *ASTGen::generate(PoundFunctionExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundFunction());
}

Expr *ASTGen::generate(UnknownExprSyntax &Expr) {
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
      SourceLoc Loc = topLoc();
      return new (Context) MagicIdentifierLiteralExpr(MagicKind, Loc);
    }
    default:
      return nullptr;
    }
  }
  return nullptr;
}
