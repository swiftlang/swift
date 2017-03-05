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

#pragma mark - statement API

StmtSyntax::StmtSyntax(const RC<SyntaxData> Root, const StmtSyntaxData *Data)
  : Syntax(Root, Data) {}

#pragma mark - unknown-statement Data

UnknownStmtSyntaxData::UnknownStmtSyntaxData(RC<RawSyntax> Raw,
                                             const SyntaxData *Parent,
                                             CursorIndex IndexInParent)
  : UnknownSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Kind == SyntaxKind::UnknownStmt);
}

RC<UnknownStmtSyntaxData>
UnknownStmtSyntaxData::make(RC<RawSyntax> Raw,
                            const SyntaxData *Parent,
                            CursorIndex IndexInParent) {
  auto UnknownRaw = RawSyntax::make(SyntaxKind::UnknownStmt, Raw->Layout,
                                    Raw->Presence);
  return RC<UnknownStmtSyntaxData> {
    new UnknownStmtSyntaxData {
      UnknownRaw, Parent, IndexInParent
    }
  };
}

#pragma mark - unknown-statement API

UnknownStmtSyntax::UnknownStmtSyntax(const RC<SyntaxData> Root,
                                     const UnknownStmtSyntaxData *Data)
  : UnknownSyntax(Root, Data) {}

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


CodeBlockStmtSyntaxData::CodeBlockStmtSyntaxData(RC<RawSyntax> Raw,
                                                 const SyntaxData *Parent,
                                                 CursorIndex IndexInParent)
  : StmtSyntaxData(Raw, Parent, IndexInParent) {
    assert(Raw->Kind == SyntaxKind::CodeBlockStmt);
    syntax_assert_child_token_text(Raw, CodeBlockStmtSyntax::Cursor::LeftBrace,
                                   tok::l_brace, "{");
    syntax_assert_child_kind(Raw, CodeBlockStmtSyntax::Cursor::Elements,
                             SyntaxKind::StmtList);
    syntax_assert_child_token_text(Raw, CodeBlockStmtSyntax::Cursor::RightBrace,
                                   tok::r_brace, "}");
}

RC<CodeBlockStmtSyntaxData>
CodeBlockStmtSyntaxData::make(RC<RawSyntax> Raw, const SyntaxData *Parent,
                              CursorIndex IndexInParent) {
  return RC<CodeBlockStmtSyntaxData> {
    new CodeBlockStmtSyntaxData { Raw, Parent, IndexInParent }
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

#pragma mark - break-statement Data

BreakStmtSyntaxData::BreakStmtSyntaxData(RC<RawSyntax> Raw,
                                         const SyntaxData *Parent,
                                         CursorIndex IndexInParent)
  : StmtSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Layout.size() == 2);
  syntax_assert_child_token_text(Raw, BreakStmtSyntax::Cursor::BreakKeyword,
                                 tok::kw_break, "break");
  syntax_assert_child_token(Raw, BreakStmtSyntax::Cursor::Label,
                            tok::identifier);
}

RC<BreakStmtSyntaxData>
BreakStmtSyntaxData::make(RC<RawSyntax> Raw, const SyntaxData *Parent,
                          CursorIndex IndexInParent) {
  return RC<BreakStmtSyntaxData> {
    new BreakStmtSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<BreakStmtSyntaxData> BreakStmtSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::BreakStmt,
                              {
                                TokenSyntax::missingToken(tok::kw_break,
                                                          "break"),
                                TokenSyntax::missingToken(tok::identifier, ""),
                              },
                              SourcePresence::Present));
}

#pragma mark - break-statement API

BreakStmtSyntax::BreakStmtSyntax(const RC<SyntaxData> Root,
                                 BreakStmtSyntaxData *Data)
  : StmtSyntax(Root, Data) {}

BreakStmtSyntax BreakStmtSyntax::make(RC<RawSyntax> Raw,
                                      const SyntaxData *Parent,
                                      CursorIndex IndexInParent) {
  auto Data = BreakStmtSyntaxData::make(Raw, Parent, IndexInParent);
  return { Data, Data.get() };
}

BreakStmtSyntax BreakStmtSyntax::makeBlank() {
  auto Data = BreakStmtSyntaxData::makeBlank();
  return { Data, Data.get() };
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

#pragma mark - continue-statement Data

ContinueStmtSyntaxData::ContinueStmtSyntaxData(RC<RawSyntax> Raw,
                                               const SyntaxData *Parent,
                                               CursorIndex IndexInParent)
  : StmtSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Layout.size() == 2);
  syntax_assert_child_token_text(Raw,
                                 ContinueStmtSyntax::Cursor::ContinueKeyword,
                                 tok::kw_continue, "continue");
  syntax_assert_child_token(Raw, ContinueStmtSyntax::Cursor::Label,
                            tok::identifier);
}

RC<ContinueStmtSyntaxData>
ContinueStmtSyntaxData::make(RC<RawSyntax> Raw, const SyntaxData *Parent,
                          CursorIndex IndexInParent) {
  return RC<ContinueStmtSyntaxData> {
    new ContinueStmtSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<ContinueStmtSyntaxData> ContinueStmtSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::ContinueStmt,
                              {
                                TokenSyntax::missingToken(tok::kw_continue,
                                                          "continue"),
                                TokenSyntax::missingToken(tok::identifier, ""),
                              },
                              SourcePresence::Present));
}

#pragma mark - continue-statement API

ContinueStmtSyntax::ContinueStmtSyntax(const RC<SyntaxData> Root,
                                 ContinueStmtSyntaxData *Data)
  : StmtSyntax(Root, Data) {}

ContinueStmtSyntax ContinueStmtSyntax::make(RC<RawSyntax> Raw,
                                      const SyntaxData *Parent,
                                      CursorIndex IndexInParent) {
  auto Data = ContinueStmtSyntaxData::make(Raw, Parent, IndexInParent);
  return { Data, Data.get() };
}

ContinueStmtSyntax ContinueStmtSyntax::makeBlank() {
  auto Data = ContinueStmtSyntaxData::makeBlank();
  return { Data, Data.get() };
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

#pragma mark - return-statement Data

ReturnStmtSyntaxData::ReturnStmtSyntaxData(RC<RawSyntax> Raw,
                                           const SyntaxData *Parent,
                                           CursorIndex IndexInParent)
  : StmtSyntaxData(Raw, Parent, IndexInParent) {
  assert(Raw->Layout.size() == 2);
  syntax_assert_child_token_text(Raw,
                                 ReturnStmtSyntax::Cursor::ReturnKeyword,
                                 tok::kw_return, "return");
  assert(Raw->getChild(ReturnStmtSyntax::Cursor::Expression)->isExpr());
}

RC<ReturnStmtSyntaxData>
ReturnStmtSyntaxData::make(RC<RawSyntax> Raw, const SyntaxData *Parent,
                             CursorIndex IndexInParent) {
  return RC<ReturnStmtSyntaxData> {
    new ReturnStmtSyntaxData {
      Raw, Parent, IndexInParent
    }
  };
}

RC<ReturnStmtSyntaxData> ReturnStmtSyntaxData::makeBlank() {
  return make(RawSyntax::make(SyntaxKind::ReturnStmt,
                              {
                                TokenSyntax::missingToken(tok::kw_return,
                                                          "return"),
                                RawSyntax::missing(SyntaxKind::MissingExpr),
                              },
                              SourcePresence::Present));
}

#pragma mark - return-statement API

ReturnStmtSyntax::ReturnStmtSyntax(const RC<SyntaxData> Root,
                                   const ReturnStmtSyntaxData *Data)
  : StmtSyntax(Root, Data) {}

ReturnStmtSyntax ReturnStmtSyntax::make(RC<RawSyntax> Raw,
                                        const SyntaxData *Parent,
                                        CursorIndex IndexInParent) {
  auto Data = ReturnStmtSyntaxData::make(Raw, Parent, IndexInParent);
  return { Data, Data.get() };
}

ReturnStmtSyntax ReturnStmtSyntax::makeBlank() {
  auto Data = ReturnStmtSyntaxData::makeBlank();
  return { Data, Data.get() };
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
  auto RawExpression = getRaw()->getChild(Cursor::Expression);
  if (RawExpression->isMissing()) {
    return llvm::None;
  }

  auto *MyData = getUnsafeData<ReturnStmtSyntax>();

  auto &ChildPtr = *reinterpret_cast<std::atomic<uintptr_t>*>(
    &MyData->CachedExpression);

  SyntaxData::realizeSyntaxNode<ExprSyntax>(ChildPtr, RawExpression, MyData,
                                            cursorIndex(Cursor::Expression));

  return ExprSyntax { Root, MyData->CachedExpression.get() };
}

ReturnStmtSyntax
ReturnStmtSyntax::withExpression(ExprSyntax NewExpression) const {
  return Data->replaceChild<ReturnStmtSyntax>(NewExpression.getRaw(),
                                              Cursor::Expression);
}


#pragma mark code-block API

CodeBlockStmtSyntax::CodeBlockStmtSyntax(const RC<SyntaxData> Root,
                                         CodeBlockStmtSyntaxData *Data)
    : StmtSyntax(Root, Data) {}
