//===--- ExprSyntax.cpp - Swift Expression Syntax Impl. ---------*- C++ -*-===//
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

#include "swift/Syntax/ExprSyntax.h"

using namespace swift;
using namespace swift::syntax;

#pragma mark - expression Data

RC<ExprSyntaxData> ExprSyntaxData::make(RC<RawSyntax> Raw,
                                        const SyntaxData *Parent,
                                        CursorIndex IndexInParent) {
  return RC<ExprSyntaxData> {
    new ExprSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<ExprSyntaxData> ExprSyntaxData::makeBlank() {
  return make(RawSyntax::missing(SyntaxKind::MissingExpr));
}

#pragma mark - expression API

ExprSyntax::ExprSyntax(const RC<SyntaxData> Root, const ExprSyntaxData *Data)
  : Syntax(Root, Data) {}

#pragma mark - unknown-expression Data

UnknownExprSyntaxData::UnknownExprSyntaxData(RC<RawSyntax> Raw,
                                             const SyntaxData *Parent,
                                             CursorIndex IndexInParent)
  : ExprSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::UnknownExpr);
}

RC<UnknownExprSyntaxData>
UnknownExprSyntaxData::make(RC<RawSyntax> Raw,
                            const SyntaxData *Parent,
                            CursorIndex IndexInParent) {
  return RC<UnknownExprSyntaxData> {
    new UnknownExprSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

#pragma mark - unknown-expression API

UnknownExprSyntax::UnknownExprSyntax(const RC<SyntaxData> Root,
                                     const UnknownExprSyntaxData *Data)
  : ExprSyntax(Root, Data) {}

#pragma mark - integer-literal-expression Data

IntegerLiteralExprSyntaxData::
IntegerLiteralExprSyntaxData(RC<RawSyntax> Raw,
                             const SyntaxData *Parent,
                             CursorIndex IndexInParent)
  : ExprSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::IntegerLiteralExpr);
  assert(Raw->Layout.size() == 2);
  syntax_assert_child_token(Raw, IntegerLiteralExprSyntax::Cursor::Sign,
                            tok::oper_prefix);
  syntax_assert_child_token(Raw, IntegerLiteralExprSyntax::Cursor::Digits,
                            tok::integer_literal);
}

RC<IntegerLiteralExprSyntaxData>
IntegerLiteralExprSyntaxData::make(RC<RawSyntax> Raw,
                                   const SyntaxData *Parent,
                                   CursorIndex IndexInParent) {
  return RC<IntegerLiteralExprSyntaxData> {
    new IntegerLiteralExprSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}
RC<IntegerLiteralExprSyntaxData> IntegerLiteralExprSyntaxData::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::IntegerLiteralExpr,
    {
      TokenSyntax::missingToken(tok::oper_prefix, ""),
      TokenSyntax::missingToken(tok::integer_literal, "")
    },
    SourcePresence::Present);
  return make(Raw);
}


#pragma mark - integer-literal-expression API

IntegerLiteralExprSyntax::
IntegerLiteralExprSyntax(const RC<SyntaxData> Root,
                         const IntegerLiteralExprSyntaxData *Data)
  : ExprSyntax(Root, Data) {}

IntegerLiteralExprSyntax
IntegerLiteralExprSyntax::withDigits(RC<TokenSyntax> NewDigits) const {
  assert(NewDigits->getTokenKind() == tok::integer_literal);
  return Data->replaceChild<IntegerLiteralExprSyntax>(NewDigits,
                                                      Cursor::Digits);
}
