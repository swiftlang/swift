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
#include "swift/AST/DiagnosticsParse.h"
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
  PersistentParserState &ParserState;
  CodeCompletionCallbacksFactory *CodeCompletionFactory;

public:
  ParseDelayedFunctionBodies(PersistentParserState &ParserState,
                             CodeCompletionCallbacksFactory *Factory)
    : ParserState(ParserState), CodeCompletionFactory(Factory) {}

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

    SourceFile &SF = *AFD->getDeclContext()->getParentSourceFile();
    SourceManager &SourceMgr = SF.getASTContext().SourceMgr;
    unsigned BufferID = SourceMgr.findBufferContainingLoc(AFD->getLoc());
    Parser TheParser(BufferID, SF, nullptr, &ParserState);

    std::unique_ptr<CodeCompletionCallbacks> CodeCompletion;
    if (CodeCompletionFactory) {
      CodeCompletion.reset(
          CodeCompletionFactory->createCodeCompletionCallbacks(TheParser));
      TheParser.setCodeCompletionCallbacks(CodeCompletion.get());
    }
    bool Parsed = false;
    if (auto FD = dyn_cast<FuncDecl>(AFD)) {
      if (FD->isAccessor()) {
        TheParser.parseAccessorBodyDelayed(AFD);
        Parsed = true;
      }
    }
    if (!Parsed)
      TheParser.parseAbstractFunctionBodyDelayed(AFD);
    if (CodeCompletion)
      CodeCompletion->doneParsing();
  }
};

static void parseDelayedDecl(
              PersistentParserState &ParserState,
              CodeCompletionCallbacksFactory *CodeCompletionFactory) {
  if (!ParserState.hasDelayedDecl())
    return;

  SourceFile &SF = *ParserState.getDelayedDeclContext()->getParentSourceFile();
  SourceManager &SourceMgr = SF.getASTContext().SourceMgr;
  unsigned BufferID =
    SourceMgr.findBufferContainingLoc(ParserState.getDelayedDeclLoc());
  Parser TheParser(BufferID, SF, nullptr, &ParserState);

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

bool swift::parseIntoSourceFile(SourceFile &SF,
                                unsigned BufferID,
                                bool *Done,
                                SILParserState *SIL,
                                PersistentParserState *PersistentState,
                                DelayedParsingCallbacks *DelayedParseCB) {
  Parser P(BufferID, SF, SIL, PersistentState);
  PrettyStackTraceParser StackTrace(P);

  if (DelayedParseCB)
    P.setDelayedParsingCallbacks(DelayedParseCB);

  bool FoundSideEffects = P.parseTopLevel();
  *Done = P.Tok.is(tok::eof);

  return FoundSideEffects;
}

void swift::performDelayedParsing(
    DeclContext *DC, PersistentParserState &PersistentState,
    CodeCompletionCallbacksFactory *CodeCompletionFactory) {
  ParseDelayedFunctionBodies Walker(PersistentState,
                                    CodeCompletionFactory);
  DC->walkContext(Walker);

  if (CodeCompletionFactory)
    parseDelayedDecl(PersistentState, CodeCompletionFactory);
}

/// \brief Tokenizes a string literal, taking into account string interpolation.
static void getStringPartTokens(const Token &Tok, const LangOptions &LangOpts,
                                const SourceManager &SM,
                                int BufID, std::vector<Token> &Toks) {
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

      StringRef Text = SM.extractText({ Loc, Len });
      Token NewTok;
      NewTok.setToken(tok::string_literal, Text);
      Toks.push_back(NewTok);

    } else {
      assert(Seg.Kind == Lexer::StringSegment::Expr &&
             "new enumerator was introduced ?");
      unsigned Offset = SM.getLocOffsetInBuffer(Seg.Loc, BufID);
      unsigned EndOffset = Offset + Seg.Length;

      if (isFirst) {
        // Add a token for the quote character.
        StringRef Text = SM.extractText({ Seg.Loc.getAdvancedLoc(-2), 1 });
        Token NewTok;
        NewTok.setToken(tok::string_literal, Text);
        Toks.push_back(NewTok);
      }

      std::vector<Token> NewTokens = swift::tokenize(LangOpts, SM, BufID,
                                                     Offset, EndOffset,
                                                     /*KeepComments=*/true);
      Toks.insert(Toks.end(), NewTokens.begin(), NewTokens.end());

      if (isLast) {
        // Add a token for the quote character.
        StringRef Text = SM.extractText({ Seg.Loc.getAdvancedLoc(Seg.Length),
                                          1 });
        Token NewTok;
        NewTok.setToken(tok::string_literal, Text);
        Toks.push_back(NewTok);
      }
    }
  }
}

std::vector<Token> swift::tokenize(const LangOptions &LangOpts,
                                   const SourceManager &SM, unsigned BufferID,
                                   unsigned Offset, unsigned EndOffset,
                                   bool KeepComments,
                                   bool TokenizeInterpolatedString) {
  if (Offset == 0 && EndOffset == 0)
    EndOffset = SM.getRangeForBuffer(BufferID).getByteLength();

  Lexer L(LangOpts, SM, BufferID, /*Diags=*/nullptr, /*InSILMode=*/false,
          KeepComments ? CommentRetentionMode::ReturnAsTokens
                       : CommentRetentionMode::AttachToNextToken,
          Offset, EndOffset);
  std::vector<Token> Tokens;
  do {
    Tokens.emplace_back();
    L.lex(Tokens.back());
    if (Tokens.back().is(tok::string_literal) && TokenizeInterpolatedString) {
      Token StrTok = Tokens.back();
      Tokens.pop_back();
      getStringPartTokens(StrTok, LangOpts, SM, BufferID, Tokens);
    }
  } while (Tokens.back().isNot(tok::eof));
  Tokens.pop_back(); // Remove EOF.
  return Tokens;
}

//===----------------------------------------------------------------------===//
// Setup and Helper Methods
//===----------------------------------------------------------------------===//

Parser::Parser(unsigned BufferID, SourceFile &SF, SILParserState *SIL,
               PersistentParserState *PersistentState)
  : Parser(std::unique_ptr<Lexer>(
             new Lexer(SF.getASTContext().LangOpts, SF.getASTContext().SourceMgr,
                   BufferID, &SF.getASTContext().Diags,
                   /*InSILMode=*/SIL != nullptr,
                   SF.getASTContext().LangOpts.AttachCommentsToDecls
                   ? CommentRetentionMode::AttachToNextToken
                   : CommentRetentionMode::None)), SF, SIL, PersistentState) {
}

Parser::Parser(std::unique_ptr<Lexer> Lex, SourceFile &SF,
               SILParserState *SIL, PersistentParserState *PersistentState)
  : SourceMgr(SF.getASTContext().SourceMgr),
    Diags(SF.getASTContext().Diags),
    SF(SF),
    L(Lex.release()),
    SIL(SIL),
    CurDeclContext(&SF),
    Context(SF.getASTContext()) {

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
      SourceMgr.findBufferContainingLoc(ParserPos.Loc) == L->getBufferID()) {
    auto BeginParserPosition = getParserPosition(ParserPos);
    restoreParserPosition(BeginParserPosition);
    InPoundLineEnvironment = State->InPoundLineEnvironment;
  }
}

Parser::~Parser() {
  delete L;
}

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

Parser::ParserPosition Parser::getParserPositionAfterFirstCharacter(Token T) {
  assert(T.getLength() > 1 && "Token must have more than one character");
  auto Loc = T.getLoc();
  auto NewState = L->getStateForBeginningOfTokenLoc(Loc.getAdvancedLoc(1));
  return ParserPosition(NewState, Loc);
}

SourceLoc Parser::consumeStartingCharacterOfCurrentToken() {
  // Consumes one-character token (like '?', '<', '>' or '!') and returns
  // it's location.

  // Current token can be either one-character token we want to consume...
  if (Tok.getLength() == 1) {
    return consumeToken();
  }

  // ... or a multi-charater token with the first caracter being the one that
  // we want to consume as a separate token.
  restoreParserPosition(getParserPositionAfterFirstCharacter(Tok));
  return PreviousLoc;
}

SourceLoc Parser::consumeStartingLess() {
  assert(startsWithLess(Tok) && "Token does not start with '<'");
  return consumeStartingCharacterOfCurrentToken();
}

SourceLoc Parser::consumeStartingGreater() {
  assert(startsWithGreater(Tok) && "Token does not start with '>'");
  return consumeStartingCharacterOfCurrentToken();
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
    if (isStartOfStmt() || isStartOfDecl())
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
         !isStartOfDecl())
    skipSingle();
}

void Parser::skipUntilDeclStmtRBrace(tok T1) {
  while (Tok.isNot(T1) && Tok.isNot(tok::eof) && Tok.isNot(tok::r_brace) &&
         !isStartOfStmt() && !isStartOfDecl()) {
    skipSingle();
  }
}

void Parser::skipUntilDeclRBrace(tok T1, tok T2) {
  while (Tok.isNot(T1) && Tok.isNot(T2) &&
         Tok.isNot(tok::eof) && Tok.isNot(tok::r_brace) &&
         !isStartOfDecl()) {
    skipSingle();
  }
}

void Parser::skipUntilConfigBlockClose() {
  while (Tok.isNot(tok::pound_else) &&
         Tok.isNot(tok::pound_elseif) &&
         Tok.isNot(tok::pound_endif) &&
         Tok.isNot(tok::eof)) {
    skipSingle();
  }
}

Parser::StructureMarkerRAII::StructureMarkerRAII(Parser &parser,
                                                 const Token &tok)
  : P(parser)
{
  switch (tok.getKind()) {
  case tok::l_brace:
    P.StructureMarkers.push_back({tok.getLoc(),
                                  StructureMarkerKind::OpenBrace,
                                  None});
    break;

  case tok::l_paren:
    P.StructureMarkers.push_back({tok.getLoc(),
                                  StructureMarkerKind::OpenParen,
                                  None});
    break;

  case tok::l_square:
    P.StructureMarkers.push_back({tok.getLoc(),
                                  StructureMarkerKind::OpenSquare,
                                  None});
    break;

  default:
    llvm_unreachable("Not a matched token");
  }
}

//===----------------------------------------------------------------------===//
// Primitive Parsing
//===----------------------------------------------------------------------===//

bool Parser::parseIdentifier(Identifier &Result, SourceLoc &Loc,
                             const Diagnostic &D) {
  switch (Tok.getKind()) {
  case tok::kw_self:
  case tok::kw_Self:
  case tok::kw_throws:
  case tok::identifier:
    Loc = consumeIdentifier(&Result);
    return false;
  default:
    checkForInputIncomplete();
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

  // When we know we're supposed to get an identifier or operator, map the
  // postfix '!' to an operator name.
  if (Tok.is(tok::exclaim_postfix)) {
    Result = Context.getIdentifier(Tok.getText());
    Loc = Tok.getLoc();
    consumeToken(tok::exclaim_postfix);
    return false;
  }

  checkForInputIncomplete();
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

  checkForInputIncomplete();
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
  default:            llvm_unreachable("unknown matching token!"); break;
  }
  if (parseToken(K, TokLoc, ErrorDiag)) {
    diagnose(OtherLoc, OtherNote);

    TokLoc = PreviousLoc;
    return true;
  }

  return false;
}

ParserStatus
Parser::parseList(tok RightK, SourceLoc LeftLoc, SourceLoc &RightLoc,
                  tok SeparatorK, bool OptionalSep, bool AllowSepAfterLast,
                  Diag<> ErrorDiag, std::function<ParserStatus()> callback) {
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
    if (consumeIf(SeparatorK)) {
      if (AllowSepAfterLast && Tok.is(RightK))
        break;
      else
        continue;
    }
    if (!OptionalSep) {
      StringRef Separator = (SeparatorK == tok::comma ? "," : ";");
      diagnose(Tok, diag::expected_separator, Separator)
        .fixItInsertAfter(PreviousLoc, Separator);
      Status.setIsParseError();
    }
    // If we haven't made progress, skip ahead
    if (Tok.getLoc() == StartLoc) {
      skipUntilDeclRBrace(RightK, SeparatorK);
      if (Tok.is(RightK))
        break;
      if (Tok.is(tok::eof)) {
        RightLoc = PreviousLoc.isValid()? PreviousLoc : Tok.getLoc();
        IsInputIncomplete = true;
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
  }

  return Status;
}

/// diagnoseRedefinition - Diagnose a redefinition error, with a note
/// referring back to the original definition.

void Parser::diagnoseRedefinition(ValueDecl *Prev, ValueDecl *New) {
  assert(New != Prev && "Cannot conflict with self");
  diagnose(New->getLoc(), diag::decl_redefinition, New->isDefinition());
  diagnose(Prev->getLoc(), diag::previous_decldef, Prev->isDefinition(),
             Prev->getName());
}

struct ParserUnit::Implementation {
  LangOptions LangOpts;
  SearchPathOptions SearchPathOpts;
  DiagnosticEngine Diags;
  ASTContext Ctx;
  SourceFile *SF;
  std::unique_ptr<Parser> TheParser;

  Implementation(SourceManager &SM, unsigned BufferID,
                 const LangOptions &Opts, StringRef ModuleName)
    : LangOpts(Opts),
      Diags(SM),
      Ctx(LangOpts, SearchPathOpts, SM, Diags),
      SF(new (Ctx) SourceFile(
            *Module::create(Ctx.getIdentifier(ModuleName), Ctx),
            SourceFileKind::Main, BufferID,
            SourceFile::ImplicitModuleImportKind::None)) {
  }
};

ParserUnit::ParserUnit(SourceManager &SM, unsigned BufferID)
  : ParserUnit(SM, BufferID, LangOptions(), "input") {
}

ParserUnit::ParserUnit(SourceManager &SM, unsigned BufferID,
                       const LangOptions &LangOpts, StringRef ModuleName)
  : Impl(*new Implementation(SM, BufferID, LangOpts, ModuleName)) {

  Impl.TheParser.reset(new Parser(BufferID, *Impl.SF, nullptr));
}

ParserUnit::ParserUnit(SourceManager &SM, unsigned BufferID,
             unsigned Offset, unsigned EndOffset)
  : Impl(*new Implementation(SM, BufferID, LangOptions(), "input")) {

  std::unique_ptr<Lexer> Lex;
  Lex.reset(new Lexer(Impl.LangOpts, SM,
                      BufferID, &Impl.Diags,
                      /*InSILMode=*/false,
                      CommentRetentionMode::None,
                      Offset, EndOffset));
  Impl.TheParser.reset(new Parser(std::move(Lex), *Impl.SF));
}

ParserUnit::~ParserUnit() {
  delete &Impl;
}

Parser &ParserUnit::getParser() {
  return *Impl.TheParser;
}

DiagnosticEngine &ParserUnit::getDiagnosticEngine() {
  return Impl.Diags;
}

const LangOptions &ParserUnit::getLangOptions() const {
  return Impl.LangOpts;
}

SourceFile &ParserUnit::getSourceFile() {
  return *Impl.SF;
}

