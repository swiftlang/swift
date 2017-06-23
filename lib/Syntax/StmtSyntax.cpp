//===--- StmtSyntax.cpp - Swift Statement Syntax Implementation -----------===//
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

#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/ExprSyntax.h"
#include "swift/Syntax/StmtSyntax.h"

using namespace swift;
using namespace swift::syntax;

#pragma mark - unknown-statement API

void UnknownStmtSyntax::validate() const {
  assert(Data->Raw->Kind == SyntaxKind::UnknownStmt);
}

#pragma mark fallthrough-statement API

FallthroughStmtSyntax FallthroughStmtSyntax::makeBlank() {
  return make<FallthroughStmtSyntax>(
    RawSyntax::make(SyntaxKind::FallthroughStmt,
    {
      TokenSyntax::missingToken(tok::kw_fallthrough, "fallthrough"),
    },
    SourcePresence::Present));
}

void FallthroughStmtSyntax::validate() const {
  assert(Data->Raw->Kind == SyntaxKind::FallthroughStmt);
  assert(Data->Raw->Layout.size() == 1);
  syntax_assert_child_token_text(Data->Raw, Cursor::FallthroughKeyword,
                                 tok::kw_fallthrough, "fallthrough");
}

RC<TokenSyntax> FallthroughStmtSyntax::getFallthroughKeyword() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::FallthroughKeyword));
}

FallthroughStmtSyntax FallthroughStmtSyntax::
withFallthroughKeyword(RC<TokenSyntax> NewFallthroughKeyword) const {
  syntax_assert_token_is(NewFallthroughKeyword, tok::kw_fallthrough,
                         "fallthrough");
  return Data->replaceChild<FallthroughStmtSyntax>(NewFallthroughKeyword,
                                                   Cursor::FallthroughKeyword);
}

#pragma mark code-block API

CodeBlockStmtSyntax
CodeBlockStmtSyntax::makeBlank() {
  return make<CodeBlockStmtSyntax>(RawSyntax::make(SyntaxKind::CodeBlockStmt,
                              {
                                TokenSyntax::missingToken(tok::l_brace, "{"),
                                RawSyntax::missing(SyntaxKind::StmtList),
                                TokenSyntax::missingToken(tok::r_brace, "}"),
                              },
                              SourcePresence::Present));
}

void CodeBlockStmtSyntax::validate() const {
  assert(Data->Raw->Kind == SyntaxKind::CodeBlockStmt);
  syntax_assert_child_token_text(Data->Raw, Cursor::LeftBrace,
                                 tok::l_brace, "{");
  syntax_assert_child_kind(Data->Raw, Cursor::Elements,
                           SyntaxKind::StmtList);
  syntax_assert_child_token_text(Data->Raw, Cursor::RightBrace,
                                 tok::r_brace, "}");
}

#pragma mark - break-statement API

void BreakStmtSyntax::validate() const {
  assert(Data->Raw->Layout.size() == 2);
  syntax_assert_child_token_text(Data->Raw, Cursor::BreakKeyword,
                                 tok::kw_break, "break");
  syntax_assert_child_token(Data->Raw, BreakStmtSyntax::Cursor::Label,
                            tok::identifier);
}

BreakStmtSyntax BreakStmtSyntax::makeBlank() {
  return make<BreakStmtSyntax>(RawSyntax::make(SyntaxKind::BreakStmt,
                               {
                                 TokenSyntax::missingToken(tok::kw_break,
                                                           "break"),
                                 TokenSyntax::missingToken(tok::identifier, ""),
                               },
                               SourcePresence::Present));
}

RC<TokenSyntax> BreakStmtSyntax::getBreakKeyword() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::BreakKeyword));
}

BreakStmtSyntax
BreakStmtSyntax::withBreakKeyword(RC<TokenSyntax> NewBreakKeyword) const {
  syntax_assert_token_is(NewBreakKeyword, tok::kw_break, "break");
  return Data->replaceChild<BreakStmtSyntax>(NewBreakKeyword,
                                             Cursor::BreakKeyword);
}

RC<TokenSyntax> BreakStmtSyntax::getLabel() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::Label));
}

BreakStmtSyntax BreakStmtSyntax::withLabel(RC<TokenSyntax> NewLabel) const {
  assert(NewLabel->getTokenKind() == tok::identifier);
  return Data->replaceChild<BreakStmtSyntax>(NewLabel, Cursor::Label);
}

ContinueStmtSyntax ContinueStmtSyntax::makeBlank() {
  return make<ContinueStmtSyntax>(
            RawSyntax::make(SyntaxKind::ContinueStmt,
            {
              TokenSyntax::missingToken(tok::kw_continue, "continue"),
              TokenSyntax::missingToken(tok::identifier, ""),
            },
            SourcePresence::Present));
}

#pragma mark - continue-statement API

void ContinueStmtSyntax::validate() const {
  assert(Data->Raw->Layout.size() == 2);
  syntax_assert_child_token_text(Data->Raw,
                                 Cursor::ContinueKeyword,
                                 tok::kw_continue, "continue");
  syntax_assert_child_token(Data->Raw, Cursor::Label,
                            tok::identifier);
}

RC<TokenSyntax> ContinueStmtSyntax::getContinueKeyword() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::ContinueKeyword));
}

ContinueStmtSyntax ContinueStmtSyntax::
withContinueKeyword(RC<TokenSyntax> NewContinueKeyword) const {
  syntax_assert_token_is(NewContinueKeyword, tok::kw_continue, "continue");
  return Data->replaceChild<ContinueStmtSyntax>(NewContinueKeyword,
                                                Cursor::ContinueKeyword);
}

RC<TokenSyntax> ContinueStmtSyntax::getLabel() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::Label));
}

ContinueStmtSyntax
ContinueStmtSyntax::withLabel(RC<TokenSyntax> NewLabel) const {
  assert(NewLabel->getTokenKind() == tok::identifier);
  return Data->replaceChild<ContinueStmtSyntax>(NewLabel, Cursor::Label);
}

#pragma mark - return-statement API

void ReturnStmtSyntax::validate() const {
  assert(Data->Raw->Layout.size() == 2);
  syntax_assert_child_token_text(Data->Raw,
                                 Cursor::ReturnKeyword,
                                 tok::kw_return, "return");
  assert(Data->Raw->getChild(Cursor::Expression)->isExpr());
}

ReturnStmtSyntax ReturnStmtSyntax::makeBlank() {
  auto Raw = RawSyntax::make(SyntaxKind::ReturnStmt,
                             {
                               TokenSyntax::missingToken(tok::kw_return,
                                                         "return"),
                               RawSyntax::missing(SyntaxKind::MissingExpr),
                             },
                             SourcePresence::Present);
  return make<ReturnStmtSyntax>(Raw);
}

RC<TokenSyntax> ReturnStmtSyntax::getReturnKeyword() const {
  return cast<TokenSyntax>(getRaw()->getChild(Cursor::ReturnKeyword));
}

ReturnStmtSyntax ReturnStmtSyntax::
withReturnKeyword(RC<TokenSyntax> NewReturnKeyword) const {
  syntax_assert_token_is(NewReturnKeyword, tok::kw_return, "return");
  return Data->replaceChild<ReturnStmtSyntax>(NewReturnKeyword,
                                              Cursor::ReturnKeyword);
}

Optional<ExprSyntax> ReturnStmtSyntax::getExpression() const {
  return ExprSyntax { Root, Data->getChild(Cursor::Expression).get() };
}

ReturnStmtSyntax
ReturnStmtSyntax::withExpression(ExprSyntax NewExpression) const {
  return Data->replaceChild<ReturnStmtSyntax>(NewExpression.getRaw(),
                                              Cursor::Expression);
}
