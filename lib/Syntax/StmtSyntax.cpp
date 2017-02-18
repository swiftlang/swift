//===--- StmtSyntax.cpp - Swift Statement Syntax Implementation -*- C++ -*-===//
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
#include "swift/Syntax/StmtSyntax.h"

using namespace swift;
using namespace swift::syntax;

StmtSyntax::StmtSyntax(const RC<SyntaxData> Root, const StmtSyntaxData *Data)
  : Syntax(Root, Data) {}

#pragma mark fallthrough-statement Data

FallthroughStmtSyntaxData::FallthroughStmtSyntaxData(RC<RawSyntax> Raw,
                                                     const SyntaxData *Parent,
                                                     CursorIndex IndexInParent)
 : StmtSyntaxData(Raw, Parent, IndexInParent) {
   assert(Raw->Kind == SyntaxKind::FallthroughStmt);
   assert(Raw->Layout.size() == 1);
   syntax_assert_child_token_text(Raw,
      FallthroughStmtSyntax::Cursor::FallthroughKeyword,
      tok::kw_fallthrough, "fallthrough");
}

RC<FallthroughStmtSyntaxData>
FallthroughStmtSyntaxData::make(RC<RawSyntax> Raw,
                                const SyntaxData *Parent,
                                CursorIndex IndexInParent) {
  return RC<FallthroughStmtSyntaxData> {
    new FallthroughStmtSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<FallthroughStmtSyntaxData> FallthroughStmtSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::FallthroughStmt,
    {
      TokenSyntax::missingToken(tok::kw_fallthrough, "fallthrough"),
    },
    SourcePresence::Present));
}

#pragma mark fallthrough-statement API

FallthroughStmtSyntax::
FallthroughStmtSyntax(const RC<SyntaxData> Root,
                      const FallthroughStmtSyntaxData *Data)
    : StmtSyntax(Root, Data) {}

FallthroughStmtSyntax
FallthroughStmtSyntax::make(RC<RawSyntax> Raw, const SyntaxData *Parent,
                            CursorIndex IndexInParent) {
  assert(Raw->Layout.size() == 1);
  syntax_assert_child_token_text(Raw,
                                 Cursor::FallthroughKeyword,
                                 tok::kw_fallthrough, "fallthrough");
  auto Data = FallthroughStmtSyntaxData::make(Raw, Parent, IndexInParent);
  return FallthroughStmtSyntax {
    Data, Data.get(),
  };
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


CodeBlockStmtSyntaxData::CodeBlockStmtSyntaxData(RC<RawSyntax> Raw)
  : StmtSyntaxData(Raw) {
    assert(Raw->Kind == SyntaxKind::CodeBlockStmt);
    syntax_assert_child_token_text(Raw, CodeBlockStmtSyntax::Cursor::LeftBrace,
                                   tok::l_brace, "{");
    syntax_assert_child_kind(Raw, CodeBlockStmtSyntax::Cursor::Elements,
                             SyntaxKind::StmtList);
    syntax_assert_child_token_text(Raw, CodeBlockStmtSyntax::Cursor::RightBrace,
                                   tok::r_brace, "}");
}

RC<CodeBlockStmtSyntaxData>
CodeBlockStmtSyntaxData::make(RC<RawSyntax> Raw) {
  return RC<CodeBlockStmtSyntaxData> {
    new CodeBlockStmtSyntaxData { Raw }
  };
}

RC<CodeBlockStmtSyntaxData>
CodeBlockStmtSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::CodeBlockStmt,
                              {
                                TokenSyntax::missingToken(tok::l_brace, "{"),
                                RawSyntax::missing(SyntaxKind::StmtList),
                                TokenSyntax::missingToken(tok::r_brace, "}"),
                              },
                              SourcePresence::Present));
}

#pragma mark code-block API

CodeBlockStmtSyntax::CodeBlockStmtSyntax(const RC<SyntaxData> Root,
                                         CodeBlockStmtSyntaxData *Data)
    : StmtSyntax(Root, Data) {}


#pragma mark statements Data

StmtListSyntaxData::StmtListSyntaxData(RC<RawSyntax> Raw,
                                       const SyntaxData *Parent,
                                       CursorIndex IndexInParent)
    : StmtSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::StmtList);
}

RC<StmtListSyntaxData> StmtListSyntaxData::make(RC<RawSyntax> Raw,
                                                const SyntaxData *Parent,
                                                CursorIndex IndexInParent) {
  return RC<StmtListSyntaxData> {
    new StmtListSyntaxData { Raw, Parent, IndexInParent }
  };
}

RC<StmtListSyntaxData> StmtListSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::StmtList,
                              {},
                              SourcePresence::Present));
}

#pragma mark statements API

StmtListSyntax::StmtListSyntax(const RC<SyntaxData> Root,
                               const StmtListSyntaxData *Data)
  : Syntax(Root, Data) {}

StmtListSyntax
StmtListSyntax::withAddedStatement(Syntax AdditionalStatement) const {
  auto Layout = getRaw()->Layout;
  Layout.push_back(AdditionalStatement.getRaw());
  auto NewRaw = RawSyntax::make(SyntaxKind::StmtList, Layout,
                                getRaw()->Presence);
  return Data->replaceSelf<StmtListSyntax>(NewRaw);
}

#pragma mark statements Builder

StmtListSyntaxBuilder &
StmtListSyntaxBuilder::addStatement(Syntax Statement) {
  StmtListLayout.push_back(Statement.getRaw());
  return *this;
}

StmtListSyntax StmtListSyntaxBuilder::build() const {
  auto Raw = RawSyntax::make(SyntaxKind::StmtList, StmtListLayout,
                             SourcePresence::Present);
  auto Data = StmtListSyntaxData::make(Raw);
  return StmtListSyntax { Data, Data.get() };
}

