//===--- Parser.cpp - Swift Language Parser -------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Swift parser.
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "swift/Subsystems.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/DelayedParsingCallbacks.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"

using namespace swift;

void DelayedParsingCallbacks::anchor() { }

namespace {
  /// To assist debugging parser crashes, tell us the location of the
  /// current token.
  class PrettyStackTraceParser : public llvm::PrettyStackTraceEntry {
    Parser &P;
  public:
    PrettyStackTraceParser(Parser &P) : P(P) {}
    void print(llvm::raw_ostream &out) const {
      out << "With parser at source location: ";
      P.Tok.getLoc().print(out, P.Context.SourceMgr);
      out << '\n';
    }
  };

/// A visitor that does delayed parsing of function bodies.
class ParseDelayedFunctionBodies : public ASTWalker {
  TranslationUnit *TU;
  PersistentParserState &ParserState;
  CodeCompletionCallbacksFactory *CodeCompletionFactory;

public:
  ParseDelayedFunctionBodies(TranslationUnit *TU,
                             PersistentParserState &ParserState,
                          CodeCompletionCallbacksFactory *CodeCompletionFactory)
    : TU(TU), ParserState(ParserState),
      CodeCompletionFactory(CodeCompletionFactory) {}

  bool walkToDeclPre(Decl *D) override {
    if (auto AFD = dyn_cast<AbstractFunctionDecl>(D)) {
      if (AFD->getBodyKind() != FuncDecl::BodyKind::Unparsed)
        return false;
      parseFunctionBody(AFD);
      return true;
    }
    return true;
  }

private:
  void parseFunctionBody(AbstractFunctionDecl *AFD) {
    assert(AFD->getBodyKind() == FuncDecl::BodyKind::Unparsed);

    unsigned BufferID =
        TU->Ctx.SourceMgr.findBufferContainingLoc(AFD->getLoc());
    Parser TheParser(BufferID, TU, nullptr, &ParserState);

    std::unique_ptr<CodeCompletionCallbacks> CodeCompletion;
    if (CodeCompletionFactory) {
      CodeCompletion.reset(
          CodeCompletionFactory->createCodeCompletionCallbacks(TheParser));
      TheParser.setCodeCompletionCallbacks(CodeCompletion.get());
    }
    TheParser.parseAbstractFunctionBodyDelayed(AFD);
    if (CodeCompletion)
      CodeCompletion->doneParsing();
  }
};

void parseDelayedDecl(TranslationUnit *TU, PersistentParserState &ParserState,
                      CodeCompletionCallbacksFactory *CodeCompletionFactory) {
  if (!ParserState.hasDelayedDecl())
    return;

  unsigned BufferID = TU->Ctx.SourceMgr
      .findBufferContainingLoc(ParserState.getDelayedDeclLoc());
  Parser TheParser(BufferID, TU, nullptr, &ParserState);

  std::unique_ptr<CodeCompletionCallbacks> CodeCompletion;
  if (CodeCompletionFactory) {
    CodeCompletion.reset(
      CodeCompletionFactory->createCodeCompletionCallbacks(TheParser));
    TheParser.setCodeCompletionCallbacks(CodeCompletion.get());
  }

  switch (ParserState.getDelayedDeclKind()) {
  case PersistentParserState::DelayedDeclKind::TopLevelCodeDecl:
    TheParser.parseTopLevelCodeDeclDelayed();
    break;

  case PersistentParserState::DelayedDeclKind::Decl:
    TheParser.parseDeclDelayed();
    break;
  }
  
  if (CodeCompletion)
    CodeCompletion->doneParsing();
}
} // unnamed namespace

/// parseIntoTranslationUnit - Entrypoint for the parser.
bool swift::parseIntoTranslationUnit(TranslationUnit *TU,
                                     unsigned BufferID,
                                     bool *Done,
                                     SILParserState *SIL,
                                     PersistentParserState *PersistentState,
                                     DelayedParsingCallbacks *DelayedParseCB) {
  Parser P(BufferID, TU, SIL, PersistentState);
  PrettyStackTraceParser StackTrace(P);

  if (DelayedParseCB)
    P.setDelayedParsingCallbacks(DelayedParseCB);

  bool FoundSideEffects = P.parseTranslationUnit(TU);
  *Done = P.Tok.is(tok::eof);

  return FoundSideEffects;
}

void swift::performDelayedParsing(
    TranslationUnit *TU, PersistentParserState &PersistentState,
    CodeCompletionCallbacksFactory *CodeCompletionFactory) {
  ParseDelayedFunctionBodies Walker(TU, PersistentState,
                                    CodeCompletionFactory);
  TU->walk(Walker);

  if (CodeCompletionFactory)
    parseDelayedDecl(TU, PersistentState, CodeCompletionFactory);
}

/// \brief Tokenizes a string literal, taking into account string interpolation.
static void getStringPartTokens(const Token &Tok, SourceManager &SM,
                                int BufID, const llvm::MemoryBuffer *Buffer,
                                std::vector<Token> &Toks) {
  assert(Tok.is(tok::string_literal));
  SmallVector<Lexer::StringSegment, 4> Segments;
  Lexer::getStringLiteralSegments(Tok, Segments, /*Diags=*/0);
  for (unsigned i = 0, e = Segments.size(); i != e; ++i) {
    Lexer::StringSegment &Seg = Segments[i];
    bool isFirst = i == 0;
    bool isLast = i == e-1;
    if (Seg.Kind == Lexer::StringSegment::Literal) {
      SourceLoc Loc = Seg.Loc;
      unsigned Len = Seg.Length;
      if (isFirst) {
        // Include the quote.
        Loc = Loc.getAdvancedLoc(-1);
        ++Len;
      }
      if (isLast) {
        // Include the quote.
        ++Len;
      }
      StringRef Text(Buffer->getBufferStart() +
                         SM.getLocOffsetInBuffer(Loc, BufID),
                     Len);
      Token NewTok;
      NewTok.setToken(tok::string_literal, Text);
      Toks.push_back(NewTok);

    } else {
      unsigned Offset = SM.getLocOffsetInBuffer(Seg.Loc, BufID);
      unsigned EndOffset = Offset + Seg.Length;
      std::vector<Token> NewTokens = swift::tokenize(SM, BufID, Offset,
                                                     EndOffset,
                                                     /*KeepComments=*/true);
      Toks.insert(Toks.end(), NewTokens.begin(), NewTokens.end());
    }
  }
}

std::vector<Token> swift::tokenize(SourceManager &SM, unsigned BufferID,
                                   unsigned Offset, unsigned EndOffset,
                                   bool KeepComments,
                                   bool TokenizeInterpolatedString) {
  auto *Buffer = SM->getMemoryBuffer(BufferID);
  if (Offset == 0 && EndOffset == 0)
    EndOffset = Buffer->getBufferSize();

  Lexer L(SM, BufferID, /*Diags=*/nullptr, /*InSILMode=*/false, KeepComments,
          Offset, EndOffset);
  std::vector<Token> Tokens;
  do {
    Tokens.emplace_back();
    L.lex(Tokens.back());
    if (Tokens.back().is(tok::string_literal) && TokenizeInterpolatedString) {
      Token StrTok = Tokens.back();
      Tokens.pop_back();
      getStringPartTokens(StrTok, SM, BufferID, Buffer, Tokens);
    }
  } while (Tokens.back().isNot(tok::eof));
  Tokens.pop_back(); // Remove EOF.
  return Tokens;
}

//===----------------------------------------------------------------------===//
// Setup and Helper Methods
//===----------------------------------------------------------------------===//

Parser::Parser(unsigned BufferID, TranslationUnit *TU, SILParserState *SIL,
               PersistentParserState *PersistentState)
  : SourceMgr(TU->getASTContext().SourceMgr),
    BufferID(BufferID),
    Diags(TU->getASTContext().Diags),
    TU(TU),
    L(new Lexer(TU->getASTContext().SourceMgr, BufferID,
                &TU->getASTContext().Diags,
                /*InSILMode=*/SIL != nullptr, /*KeepComments=*/false)),
    SIL(SIL),
    Component(TU->getComponent()),
    Context(TU->getASTContext()) {

  State = PersistentState;
  if (!State) {
    OwnedState.reset(new PersistentParserState());
    State = OwnedState.get();
  }

  // Set the token to a sentinel so that we know the lexer isn't primed yet.
  // This cannot be tok::unknown, since that is a token the lexer could produce.
  Tok.setKind(tok::NUM_TOKENS);

  auto ParserPos = State->takeParserPosition();
  if (ParserPos.isValid() &&
      SourceMgr.findBufferContainingLoc(ParserPos.Loc) == BufferID) {
    auto BeginParserPosition = getParserPosition(ParserPos);
    restoreParserPosition(BeginParserPosition);
  }
}

Parser::~Parser() {
  delete L;
}

/// peekToken - Return the next token that will be installed by consumeToken.
const Token &Parser::peekToken() {
  return L->peekNextToken();
}

SourceLoc Parser::consumeToken() {
  SourceLoc Loc = Tok.getLoc();
  assert(Tok.isNot(tok::eof) && "Lexing past eof!");
  L->lex(Tok);
  PreviousLoc = Loc;
  return Loc;
}

SourceLoc Parser::getEndOfPreviousLoc() {
  return Lexer::getLocForEndOfToken(SourceMgr, PreviousLoc);
}

SourceLoc Parser::consumeStartingLess() {
  assert(startsWithLess(Tok) && "Token does not start with '<'");
  
  if (Tok.getLength() == 1)
    return consumeToken();
  
  // Skip the starting '<' in the existing token.
  SourceLoc Loc = Tok.getLoc();
  Tok = L->getTokenAt(Loc.getAdvancedLoc(1));
  return Loc;
}

SourceLoc Parser::consumeStartingGreater() {
  assert(startsWithGreater(Tok) && "Token does not start with '>'");
  
  if (Tok.getLength() == 1)
    return consumeToken();
  
  // Skip the starting '>' in the existing token.
  SourceLoc Loc = Tok.getLoc();
  Tok = L->getTokenAt(Loc.getAdvancedLoc(1));
  return Loc;
}

void Parser::skipSingle() {
  switch (Tok.getKind()) {
  case tok::l_paren:
    consumeToken();
    skipUntil(tok::r_paren);
    consumeIf(tok::r_paren);
    break;
  case tok::l_brace:
    consumeToken();
    skipUntil(tok::r_brace);
    consumeIf(tok::r_brace);
    break;
  case tok::l_square:
    consumeToken();
    skipUntil(tok::r_square);
    consumeIf(tok::r_square);
    break;
  default:
    consumeToken();
    break;
  }
}

void Parser::skipUntil(tok T1, tok T2) {
  // tok::unknown is a sentinel that means "don't skip".
  if (T1 == tok::unknown && T2 == tok::unknown) return;
  
  while (Tok.isNot(tok::eof) && Tok.isNot(T1) && Tok.isNot(T2))
    skipSingle();
}

void Parser::skipUntilAnyOperator() {
  while (Tok.isNot(tok::eof) && Tok.isNotAnyOperator())
    skipSingle();
}

void Parser::skipUntilGreaterInTypeList() {
  while (true) {
    switch (Tok.getKind()) {
    case tok::eof:
    case tok::l_brace:
    case tok::r_brace:
      return;

#define KEYWORD(X) case tok::kw_##X:
#include "swift/Parse/Tokens.def"
    // 'Self' can appear in types, skip it.
    if (Tok.is(tok::kw_Self))
      break;
    if (isStartOfStmt(Tok) || isStartOfDecl(Tok, peekToken()))
      return;
    break;

    default:
      if (Tok.isAnyOperator() && startsWithGreater(Tok))
        return;
      // Skip '[' ']' '(' ')', because they can appear in types.
      break;
    }
    skipSingle();
  }
}

void Parser::skipUntilDeclRBrace() {
  while (Tok.isNot(tok::eof) && Tok.isNot(tok::r_brace) &&
         !isStartOfDecl(Tok, peekToken()))
    skipSingle();
}

void Parser::skipUntilDeclStmtRBrace(tok T1) {
  while (Tok.isNot(T1) && Tok.isNot(tok::eof) && Tok.isNot(tok::r_brace) &&
         !isStartOfStmt(Tok) &&
         !isStartOfDecl(Tok, peekToken())) {
    skipSingle();
  }
}

void Parser::skipUntilDeclRBrace(tok T1, tok T2) {
  while (Tok.isNot(T1) && Tok.isNot(T2) &&
         Tok.isNot(tok::eof) && Tok.isNot(tok::r_brace) &&
         !isStartOfDecl(Tok, peekToken())) {
    skipSingle();
  }
}

//===----------------------------------------------------------------------===//
// Primitive Parsing
//===----------------------------------------------------------------------===//

bool Parser::parseIdentifier(Identifier &Result, SourceLoc &Loc,
                             const Diagnostic &D) {
  switch (Tok.getKind()) {
  case tok::identifier:
    Result = Context.getIdentifier(Tok.getText());
    Loc = Tok.getLoc();
    consumeToken();
    return false;
  default:
    diagnose(Tok, D);
    return true;
  }
}

/// parseAnyIdentifier - Consume an identifier or operator if present and return
/// its name in Result.  Otherwise, emit an error and return true.
bool Parser::parseAnyIdentifier(Identifier &Result, SourceLoc &Loc,
                                const Diagnostic &D) {
  if (Tok.is(tok::identifier) || Tok.isAnyOperator()) {
    Result = Context.getIdentifier(Tok.getText());
    Loc = Tok.getLoc();
    consumeToken();
    return false;
  }
  
  diagnose(Tok, D);
  return true;
}

/// parseToken - The parser expects that 'K' is next in the input.  If so, it is
/// consumed and false is returned.
///
/// If the input is malformed, this emits the specified error diagnostic.
bool Parser::parseToken(tok K, SourceLoc &TokLoc, const Diagnostic &D) {
  if (Tok.is(K)) {
    TokLoc = consumeToken(K);
    return false;
  }
  
  diagnose(Tok, D);
  return true;
}

/// parseMatchingToken - Parse the specified expected token and return its
/// location on success.  On failure, emit the specified error diagnostic, and a
/// note at the specified note location.
bool Parser::parseMatchingToken(tok K, SourceLoc &TokLoc, Diag<> ErrorDiag,
                                SourceLoc OtherLoc) {
  Diag<> OtherNote;
  switch (K) {
  case tok::r_paren:  OtherNote = diag::opening_paren;    break;
  case tok::r_square: OtherNote = diag::opening_bracket;  break;
  case tok::r_brace:  OtherNote = diag::opening_brace;    break;
  default:            assert(!"unknown matching token!"); break;
  }
  if (parseToken(K, TokLoc, ErrorDiag)) {
    diagnose(OtherLoc, OtherNote);
    return true;
  }

  return false;
}

ParserStatus
Parser::parseList(tok RightK, SourceLoc LeftLoc, SourceLoc &RightLoc,
                  tok SeparatorK, bool OptionalSep, Diag<> ErrorDiag,
                  std::function<ParserStatus()> callback) {
  assert(SeparatorK == tok::comma || SeparatorK == tok::semi);

  if (Tok.is(RightK)) {
    RightLoc = consumeToken(RightK);
    return makeParserSuccess();
  }

  ParserStatus Status;
  while (true) {
    while (Tok.is(SeparatorK)) {
      diagnose(Tok, diag::unexpected_separator,
               SeparatorK == tok::comma ? "," : ";")
        .fixItRemove(SourceRange(Tok.getLoc()));
      consumeToken();
    }
    SourceLoc StartLoc = Tok.getLoc();
    Status |= callback();
    if (Tok.is(RightK))
      break;
    // If the lexer stopped with an EOF token whose spelling is ")", then this
    // is actually the tuple that is a string literal interpolation context.
    // Just accept the ")" and build the tuple as we usually do.
    if (Tok.is(tok::eof) && Tok.getText() == ")") {
      RightLoc = Tok.getLoc();
      return Status;
    }
    if (consumeIf(SeparatorK))
      continue;
    if (!OptionalSep) {
      SourceLoc InsertLoc = Lexer::getLocForEndOfToken(SourceMgr, PreviousLoc);
      StringRef Separator = (SeparatorK == tok::comma ? "," : ";");
      diagnose(Tok, diag::expected_separator, Separator)
        .fixItInsert(InsertLoc, Separator);
      Status.setIsParseError();
    }
    // If we haven't made progress, skip ahead
    if (Tok.getLoc() == StartLoc) {
      skipUntilDeclRBrace(RightK, SeparatorK);
      if (Tok.is(RightK))
        break;
      if (Tok.is(tok::eof)) {
        RightLoc = PreviousLoc;
        Status.setIsParseError();
        return Status;
      }
      if (consumeIf(SeparatorK) || OptionalSep)
        continue;
      break;
    }
  }

  if (parseMatchingToken(RightK, RightLoc, ErrorDiag, LeftLoc)) {
    Status.setIsParseError();
    RightLoc = PreviousLoc;
  }

  return Status;
}

bool Parser::parseList(tok RightK, SourceLoc LeftLoc, SourceLoc &RightLoc,
                       tok SeparatorK, bool OptionalSep, Diag<> ErrorDiag,
                       std::function<bool()> callback) {
  return parseList(RightK, LeftLoc, RightLoc, SeparatorK, OptionalSep,
                   ErrorDiag, [&]() -> ParserStatus {
    if (callback())
      return makeParserError();
    else
      return makeParserSuccess();
  }).isError();
}

/// diagnoseRedefinition - Diagnose a redefinition error, with a note
/// referring back to the original definition.

void Parser::diagnoseRedefinition(ValueDecl *Prev, ValueDecl *New) {
  assert(New != Prev && "Cannot conflict with self");
  diagnose(New->getLoc(), diag::decl_redefinition, New->isDefinition());
  diagnose(Prev->getLoc(), diag::previous_decldef, Prev->isDefinition(),
             Prev->getName());
}
