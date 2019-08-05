//===--- ASTGen.cpp -------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/ASTGen.h"

using namespace swift;
using namespace swift::syntax;

IntegerLiteralExpr *ASTGen::generate(IntegerLiteralExprSyntax &Expr) {
  TokenSyntax Digits = Expr.getDigits();
  StringRef Text = copyAndStripUnderscores(Digits.getText());
  SourceLoc Loc = topLoc();
  return new (Context) IntegerLiteralExpr(Text, Loc);
}

FloatLiteralExpr *ASTGen::generate(FloatLiteralExprSyntax &Expr) {
  TokenSyntax FloatingDigits = Expr.getFloatingDigits();
  StringRef Text = copyAndStripUnderscores(FloatingDigits.getText());
  SourceLoc Loc = topLoc();
  return new (Context) FloatLiteralExpr(Text, Loc);
}

NilLiteralExpr *ASTGen::generate(NilLiteralExprSyntax &Expr) {
  TokenSyntax Nil = Expr.getNilKeyword();
  SourceLoc Loc = topLoc();
  return new (Context) NilLiteralExpr(Loc);
}

BooleanLiteralExpr *ASTGen::generate(BooleanLiteralExprSyntax &Expr) {
  TokenSyntax Literal = Expr.getBooleanLiteral();
  bool Value = Literal.getTokenKind() == tok::kw_true;
  SourceLoc Loc = topLoc();
  return new (Context) BooleanLiteralExpr(Value, Loc);
}

MagicIdentifierLiteralExpr *ASTGen::generate(PoundFileExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundFile());
}

MagicIdentifierLiteralExpr *ASTGen::generate(PoundLineExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundLine());
}

MagicIdentifierLiteralExpr *ASTGen::generate(PoundColumnExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundColumn());
}

MagicIdentifierLiteralExpr *ASTGen::generate(PoundFunctionExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundFunction());
}

MagicIdentifierLiteralExpr *ASTGen::generate(PoundDsohandleExprSyntax &Expr) {
  return generateMagicIdentifierLiteralExpr(Expr.getPoundDsohandle());
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

void ASTGen::pushLoc(SourceLoc Loc) { LocStack.push_back(Loc); }

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

StringRef ASTGen::copyAndStripUnderscores(StringRef Orig) {
  return copyAndStripUnderscores(Orig, Context);
}

SourceLoc ASTGen::topLoc() {
  // todo [gsoc]: create SourceLoc by pointing the offset of Syntax node into
  // the source buffer
  return LocStack.back();
}

MagicIdentifierLiteralExpr *
ASTGen::generateMagicIdentifierLiteralExpr(const TokenSyntax &PoundToken) {
  auto Kind = getMagicIdentifierLiteralKind(PoundToken.getTokenKind());
  SourceLoc Loc = topLoc();
  return new (Context) MagicIdentifierLiteralExpr(Kind, Loc);
}

MagicIdentifierLiteralExpr::Kind
ASTGen::getMagicIdentifierLiteralKind(tok Kind) {
  switch (Kind) {
  case tok::kw___COLUMN__:
  case tok::pound_column:
    return MagicIdentifierLiteralExpr::Kind::Column;
  case tok::kw___FILE__:
  case tok::pound_file:
    return MagicIdentifierLiteralExpr::Kind::File;
  case tok::kw___FUNCTION__:
  case tok::pound_function:
    return MagicIdentifierLiteralExpr::Kind::Function;
  case tok::kw___LINE__:
  case tok::pound_line:
    return MagicIdentifierLiteralExpr::Kind::Line;
  case tok::kw___DSO_HANDLE__:
  case tok::pound_dsohandle:
    return MagicIdentifierLiteralExpr::Kind::DSOHandle;
  default:
    llvm_unreachable("not a magic literal");
  }
}
