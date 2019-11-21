//===--- Parser.cpp - Swift Language Parser -------------------------------===//
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
//
//  This file implements the Swift parser.
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "swift/Subsystems.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/Module.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Timer.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/ParseSILSupport.h"
#include "swift/Parse/SyntaxParseActions.h"
#include "swift/Parse/SyntaxParsingContext.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/TokenSyntax.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"

static void getStringPartTokens(const swift::Token &Tok,
                                const swift::LangOptions &LangOpts,
                                const swift::SourceManager &SM, int BufID,
                                std::vector<swift::Token> &Toks);

namespace swift {
template <typename DF>
void tokenize(const LangOptions &LangOpts, const SourceManager &SM,
              unsigned BufferID, unsigned Offset, unsigned EndOffset,
              DiagnosticEngine * Diags,
              CommentRetentionMode RetainComments,
              TriviaRetentionMode TriviaRetention,
              bool TokenizeInterpolatedString, ArrayRef<Token> SplitTokens,
              DF &&DestFunc) {
  assert((TriviaRetention != TriviaRetentionMode::WithTrivia ||
          !TokenizeInterpolatedString) &&
         "string interpolation with trivia is not implemented yet");

  if (Offset == 0 && EndOffset == 0)
    EndOffset = SM.getRangeForBuffer(BufferID).getByteLength();

  Lexer L(LangOpts, SM, BufferID, Diags, LexerMode::Swift,
          HashbangMode::Allowed, RetainComments, TriviaRetention, Offset,
          EndOffset);

  auto TokComp = [&](const Token &A, const Token &B) {
    return SM.isBeforeInBuffer(A.getLoc(), B.getLoc());
  };

  std::set<Token, decltype(TokComp)> ResetTokens(TokComp);
  for (auto C = SplitTokens.begin(), E = SplitTokens.end(); C != E; ++C) {
    ResetTokens.insert(*C);
  }

  Token Tok;
  ParsedTrivia LeadingTrivia, TrailingTrivia;
  do {
    L.lex(Tok, LeadingTrivia, TrailingTrivia);

    // If the token has the same location as a reset location,
    // reset the token stream
    auto F = ResetTokens.find(Tok);
    if (F != ResetTokens.end()) {
      assert(F->isNot(tok::string_literal));

      DestFunc(*F, ParsedTrivia(), ParsedTrivia());

      auto NewState = L.getStateForBeginningOfTokenLoc(
          F->getLoc().getAdvancedLoc(F->getLength()));
      L.restoreState(NewState);
      continue;
    }

    if (Tok.is(tok::string_literal) && TokenizeInterpolatedString) {
      std::vector<Token> StrTokens;
      getStringPartTokens(Tok, LangOpts, SM, BufferID, StrTokens);
      for (auto &StrTok : StrTokens) {
        DestFunc(StrTok, ParsedTrivia(), ParsedTrivia());
      }
    } else {
      DestFunc(Tok, LeadingTrivia, TrailingTrivia);
    }

  } while (Tok.getKind() != tok::eof);
}
} // namespace swift

using namespace swift;
using namespace swift::syntax;

void SILParserTUStateBase::anchor() { }

void swift::performCodeCompletionSecondPass(
    PersistentParserState &ParserState,
    CodeCompletionCallbacksFactory &Factory) {
  Parser::performCodeCompletionSecondPass(ParserState, Factory);
}

void Parser::performCodeCompletionSecondPass(
    PersistentParserState &ParserState,
    CodeCompletionCallbacksFactory &Factory) {
  if (!ParserState.hasCodeCompletionDelayedDeclState())
    return;

  auto state = ParserState.takeCodeCompletionDelayedDeclState();
  auto &SF = *state->ParentContext->getParentSourceFile();
  auto &Ctx = SF.getASTContext();

  FrontendStatsTracer tracer(Ctx.Stats, "CodeCompletionSecondPass");

  auto BufferID = Ctx.SourceMgr.findBufferContainingLoc(state->BodyPos.Loc);
  Parser TheParser(BufferID, SF, nullptr, &ParserState, nullptr);

  std::unique_ptr<CodeCompletionCallbacks> CodeCompletion(
      Factory.createCodeCompletionCallbacks(TheParser));
  TheParser.setCodeCompletionCallbacks(CodeCompletion.get());

  TheParser.performCodeCompletionSecondPassImpl(*state);
}

void Parser::performCodeCompletionSecondPassImpl(
    PersistentParserState::CodeCompletionDelayedDeclState &info) {
  // Disable libSyntax creation in the delayed parsing.
  SyntaxContext->disable();

  auto BeginParserPosition = getParserPosition(info.BodyPos);
  auto EndLexerState = L->getStateForEndOfTokenLoc(info.BodyEnd);

  // ParserPositionRAII needs a primed parser to restore to.
  if (Tok.is(tok::NUM_TOKENS))
    consumeTokenWithoutFeedingReceiver();

  // Ensure that we restore the parser state at exit.
  ParserPositionRAII PPR(*this);

  // Create a lexer that cannot go past the end state.
  Lexer LocalLex(*L, BeginParserPosition.LS, EndLexerState);

  // Temporarily swap out the parser's current lexer with our new one.
  llvm::SaveAndRestore<Lexer *> T(L, &LocalLex);

  // Rewind to the beginning of the top-level code.
  restoreParserPosition(BeginParserPosition);

  // Do not delay parsing in the second pass.
  llvm::SaveAndRestore<bool> DisableDelayedBody(DelayBodyParsing, false);

  // Re-enter the lexical scope.
  Scope S(this, info.takeScope());

  DeclContext *DC = info.ParentContext;

  switch (info.Kind) {
  case PersistentParserState::CodeCompletionDelayedDeclKind::TopLevelCodeDecl: {
    // Re-enter the top-level code decl context.
    // FIXME: this can issue discriminators out-of-order?
    auto *TLCD = cast<TopLevelCodeDecl>(DC);
    ContextChange CC(*this, TLCD, &State->getTopLevelContext());

    SourceLoc StartLoc = Tok.getLoc();
    ASTNode Result;
    parseExprOrStmt(Result);
    if (!Result.isNull()) {
      auto Brace = BraceStmt::create(Context, StartLoc, Result, Tok.getLoc());
      TLCD->setBody(Brace);
    }
    break;
  }

  case PersistentParserState::CodeCompletionDelayedDeclKind::Decl: {
    assert((DC->isTypeContext() || DC->isModuleScopeContext()) &&
           "Delayed decl must be a type member or a top-level decl");
    ContextChange CC(*this, DC);

    parseDecl(ParseDeclOptions(info.Flags),
              /*IsAtStartOfLineOrPreviousHadSemi=*/true, [&](Decl *D) {
                if (auto *NTD = dyn_cast<NominalTypeDecl>(DC)) {
                  NTD->addMember(D);
                } else if (auto *ED = dyn_cast<ExtensionDecl>(DC)) {
                  ED->addMember(D);
                } else if (auto *SF = dyn_cast<SourceFile>(DC)) {
                  SF->Decls.push_back(D);
                } else {
                  llvm_unreachable("invalid decl context kind");
                }
              });
    break;
  }

  case PersistentParserState::CodeCompletionDelayedDeclKind::FunctionBody: {
    auto *AFD = cast<AbstractFunctionDecl>(DC);
    ParseFunctionBody CC(*this, AFD);
    setLocalDiscriminatorToParamList(AFD->getParameters());

    auto result = parseBraceItemList(diag::func_decl_without_brace);
    AFD->setBody(result.getPtrOrNull());
    break;
  }
  }

  assert(!State->hasCodeCompletionDelayedDeclState() &&
         "Second pass should not set any code completion info");

  CodeCompletion->doneParsing();
}

swift::Parser::BacktrackingScope::~BacktrackingScope() {
  if (Backtrack) {
    P.backtrackToPosition(PP);
    DT.abort();
  }
}

/// Tokenizes a string literal, taking into account string interpolation.
static void getStringPartTokens(const Token &Tok, const LangOptions &LangOpts,
                                const SourceManager &SM,
                                int BufID, std::vector<Token> &Toks) {
  assert(Tok.is(tok::string_literal));
  bool IsMultiline = Tok.isMultilineString();
  unsigned CustomDelimiterLen = Tok.getCustomDelimiterLen();
  unsigned QuoteLen = (IsMultiline ? 3 : 1) + CustomDelimiterLen;
  SmallVector<Lexer::StringSegment, 4> Segments;
  Lexer::getStringLiteralSegments(Tok, Segments, /*Diags=*/nullptr);
  for (unsigned i = 0, e = Segments.size(); i != e; ++i) {
    Lexer::StringSegment &Seg = Segments[i];
    bool isFirst = i == 0;
    bool isLast = i == e-1;
    if (Seg.Kind == Lexer::StringSegment::Literal) {
      SourceLoc Loc = Seg.Loc;
      unsigned Len = Seg.Length;
      if (isFirst) {
        // Include the quote.
        Loc = Loc.getAdvancedLoc(-QuoteLen);
        Len += QuoteLen;
      }
      if (isLast) {
        // Include the quote.
        Len += QuoteLen;
      }

      StringRef Text = SM.extractText({ Loc, Len });
      Token NewTok;
      NewTok.setToken(tok::string_literal, Text);
      NewTok.setStringLiteral(IsMultiline, CustomDelimiterLen);
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
                                                     /*Diags=*/nullptr,
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
                                   DiagnosticEngine *Diags,
                                   bool KeepComments,
                                   bool TokenizeInterpolatedString,
                                   ArrayRef<Token> SplitTokens) {
  std::vector<Token> Tokens;

  tokenize(LangOpts, SM, BufferID, Offset, EndOffset,
           Diags,
           KeepComments ? CommentRetentionMode::ReturnAsTokens
                        : CommentRetentionMode::AttachToNextToken,
           TriviaRetentionMode::WithoutTrivia, TokenizeInterpolatedString,
           SplitTokens,
           [&](const Token &Tok, const ParsedTrivia &LeadingTrivia,
               const ParsedTrivia &TrailingTrivia) { Tokens.push_back(Tok); });

  assert(Tokens.back().is(tok::eof));
  Tokens.pop_back(); // Remove EOF.
  return Tokens;
}

std::vector<std::pair<RC<syntax::RawSyntax>, syntax::AbsolutePosition>>
swift::tokenizeWithTrivia(const LangOptions &LangOpts, const SourceManager &SM,
                          unsigned BufferID, unsigned Offset,
                          unsigned EndOffset,
                          DiagnosticEngine *Diags) {
  std::vector<std::pair<RC<syntax::RawSyntax>, syntax::AbsolutePosition>>
      Tokens;
  syntax::AbsolutePosition RunningPos;

  tokenize(
      LangOpts, SM, BufferID, Offset, EndOffset, Diags,
      CommentRetentionMode::AttachToNextToken, TriviaRetentionMode::WithTrivia,
      /*TokenizeInterpolatedString=*/false,
      /*SplitTokens=*/ArrayRef<Token>(),
      [&](const Token &Tok, const ParsedTrivia &LeadingTrivia,
          const ParsedTrivia &TrailingTrivia) {
        CharSourceRange TokRange = Tok.getRange();
        SourceLoc LeadingTriviaLoc =
          TokRange.getStart().getAdvancedLoc(-LeadingTrivia.getLength());
        SourceLoc TrailingTriviaLoc =
          TokRange.getStart().getAdvancedLoc(TokRange.getByteLength());
        Trivia syntaxLeadingTrivia =
          LeadingTrivia.convertToSyntaxTrivia(LeadingTriviaLoc, SM, BufferID);
        Trivia syntaxTrailingTrivia =
          TrailingTrivia.convertToSyntaxTrivia(TrailingTriviaLoc, SM, BufferID);
        auto Text = OwnedString::makeRefCounted(Tok.getRawText());
        auto ThisToken =
            RawSyntax::make(Tok.getKind(), Text, syntaxLeadingTrivia.Pieces,
                            syntaxTrailingTrivia.Pieces,
                            SourcePresence::Present);

        auto ThisTokenPos = ThisToken->accumulateAbsolutePosition(RunningPos);
        Tokens.push_back({ThisToken, ThisTokenPos.getValue()});
      });

  return Tokens;
}

//===----------------------------------------------------------------------===//
// Setup and Helper Methods
//===----------------------------------------------------------------------===//


static LexerMode sourceFileKindToLexerMode(SourceFileKind kind) {
  switch (kind) {
    case swift::SourceFileKind::Interface:
      return LexerMode::SwiftInterface;
    case swift::SourceFileKind::SIL:
      return LexerMode::SIL;
    case swift::SourceFileKind::Library:
    case swift::SourceFileKind::Main:
    case swift::SourceFileKind::REPL:
      return LexerMode::Swift;
  }
  llvm_unreachable("covered switch");
}

Parser::Parser(unsigned BufferID, SourceFile &SF, SILParserTUStateBase *SIL,
               PersistentParserState *PersistentState,
               std::shared_ptr<SyntaxParseActions> SPActions,
               bool DelayBodyParsing)
    : Parser(BufferID, SF, &SF.getASTContext().Diags, SIL, PersistentState,
             std::move(SPActions), DelayBodyParsing) {}

Parser::Parser(unsigned BufferID, SourceFile &SF, DiagnosticEngine* LexerDiags,
               SILParserTUStateBase *SIL,
               PersistentParserState *PersistentState,
               std::shared_ptr<SyntaxParseActions> SPActions,
               bool DelayBodyParsing)
    : Parser(
          std::unique_ptr<Lexer>(new Lexer(
              SF.getASTContext().LangOpts, SF.getASTContext().SourceMgr,
              BufferID, LexerDiags,
              sourceFileKindToLexerMode(SF.Kind),
              SF.Kind == SourceFileKind::Main
                  ? HashbangMode::Allowed
                  : HashbangMode::Disallowed,
              SF.getASTContext().LangOpts.AttachCommentsToDecls
                  ? CommentRetentionMode::AttachToNextToken
                  : CommentRetentionMode::None,
              SF.shouldBuildSyntaxTree()
                  ? TriviaRetentionMode::WithTrivia
                  : TriviaRetentionMode::WithoutTrivia)),
          SF, SIL, PersistentState, std::move(SPActions), DelayBodyParsing) {}

namespace {

/// This is the token receiver that helps SourceFile to keep track of its
/// underlying corrected token stream.
class TokenRecorder: public ConsumeTokenReceiver {
  ASTContext &Ctx;
  SourceManager &SM;

  // Token list ordered by their appearance in the source file.
  std::vector<Token> &Bag;
  unsigned BufferID;

  // Registered token kind change. These changes are regiestered before the
  // token is consumed, so we need to keep track of them here.
  llvm::DenseMap<const void*, tok> TokenKindChangeMap;

  std::vector<Token>::iterator lower_bound(SourceLoc Loc) {
    return token_lower_bound(Bag, Loc);
  }

  std::vector<Token>::iterator lower_bound(Token Tok) {
    return lower_bound(Tok.getLoc());
  }

  void relexComment(CharSourceRange CommentRange,
                    llvm::SmallVectorImpl<Token> &Scratch) {
    Lexer L(Ctx.LangOpts, Ctx.SourceMgr, BufferID, nullptr, LexerMode::Swift,
            HashbangMode::Disallowed,
            CommentRetentionMode::ReturnAsTokens,
            TriviaRetentionMode::WithoutTrivia,
            SM.getLocOffsetInBuffer(CommentRange.getStart(), BufferID),
            SM.getLocOffsetInBuffer(CommentRange.getEnd(), BufferID));
    while(true) {
      Token Result;
      L.lex(Result);
      if (Result.is(tok::eof))
        break;
      assert(Result.is(tok::comment));
      Scratch.push_back(Result);
    }
  }

public:
  TokenRecorder(SourceFile &SF):
  Ctx(SF.getASTContext()),
  SM(SF.getASTContext().SourceMgr),
  Bag(SF.getTokenVector()),
  BufferID(SF.getBufferID().getValue()) {};

  void finalize() override {

    // We should consume the comments at the end of the file that don't attach
    // to any tokens.
    SourceLoc TokEndLoc;
    if (!Bag.empty()) {
      Token Last = Bag.back();
      TokEndLoc = Last.getLoc().getAdvancedLoc(Last.getLength());
    } else {

      // Special case: the file contains nothing but comments.
      TokEndLoc = SM.getLocForBufferStart(BufferID);
    }
    llvm::SmallVector<Token, 4> Scratch;
    relexComment(CharSourceRange(SM, TokEndLoc,
                                 SM.getRangeForBuffer(BufferID).getEnd()),
                 Scratch);
    // Accept these orphaned comments.
    Bag.insert(Bag.end(), Scratch.begin(), Scratch.end());
  }

  void registerTokenKindChange(SourceLoc Loc, tok NewKind) override {
    // If a token with the same location is already in the bag, update its kind.
    auto Pos = lower_bound(Loc);
    if (Pos != Bag.end() && Pos->getLoc().getOpaquePointerValue() ==
        Loc.getOpaquePointerValue()) {
      Pos->setKind(NewKind);
      return;
    }

    // Save the update for later.
    TokenKindChangeMap[Loc.getOpaquePointerValue()] = NewKind;
  }

  void receive(Token Tok) override {
    // We filter out all tokens without valid location
    if(Tok.getLoc().isInvalid())
      return;

    // If a token with the same location is already in the bag, skip this token.
    auto Pos = lower_bound(Tok);
    if (Pos != Bag.end() && Pos->getLoc().getOpaquePointerValue() ==
        Tok.getLoc().getOpaquePointerValue()) {
      return;
    }

    // Update Token kind if a kind update was regiestered before.
    auto Found = TokenKindChangeMap.find(Tok.getLoc().
                                         getOpaquePointerValue());
    if (Found != TokenKindChangeMap.end()) {
      Tok.setKind(Found->getSecond());
    }

    // If the token has comment attached to it, re-lexing these comments and
    // consume them as separate tokens.
    llvm::SmallVector<Token, 4> TokensToConsume;
    if (Tok.hasComment()) {
      relexComment(Tok.getCommentRange(), TokensToConsume);
    }

    TokensToConsume.push_back(Tok);
    Bag.insert(Pos, TokensToConsume.begin(), TokensToConsume.end());
  }
};
} // End of an anonymous namespace.

Parser::Parser(std::unique_ptr<Lexer> Lex, SourceFile &SF,
               SILParserTUStateBase *SIL,
               PersistentParserState *PersistentState,
               std::shared_ptr<SyntaxParseActions> SPActions,
               bool DelayBodyParsing)
  : SourceMgr(SF.getASTContext().SourceMgr),
    Diags(SF.getASTContext().Diags),
    SF(SF),
    L(Lex.release()),
    SIL(SIL),
    CurDeclContext(&SF),
    Context(SF.getASTContext()),
    DelayBodyParsing(DelayBodyParsing),
    TokReceiver(SF.shouldCollectToken() ?
                new TokenRecorder(SF) :
                new ConsumeTokenReceiver()),
    SyntaxContext(new SyntaxParsingContext(SyntaxContext, SF,
                                           L->getBufferID(),
                                           std::move(SPActions))) {
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
      L->isStateForCurrentBuffer(ParserPos.LS)) {
    restoreParserPosition(ParserPos);
    InPoundLineEnvironment = State->InPoundLineEnvironment;
  }
}

Parser::~Parser() {
  delete L;
  delete TokReceiver;
  delete SyntaxContext;
}

bool Parser::allowTopLevelCode() const {
  return SF.isScriptMode();
}

const Token &Parser::peekToken() {
  return L->peekNextToken();
}

SourceLoc Parser::consumeTokenWithoutFeedingReceiver() {
  SourceLoc Loc = Tok.getLoc();
  assert(Tok.isNot(tok::eof) && "Lexing past eof!");

  if (IsParsingInterfaceTokens && !Tok.getText().empty()) {
    SF.recordInterfaceToken(Tok.getText());
  }
  L->lex(Tok, LeadingTrivia, TrailingTrivia);
  PreviousLoc = Loc;
  return Loc;
}

void Parser::consumeExtraToken(Token Extra) {
  TokReceiver->receive(Extra);
}

SourceLoc Parser::consumeToken() {
  TokReceiver->receive(Tok);
  SyntaxContext->addToken(Tok, LeadingTrivia, TrailingTrivia);
  return consumeTokenWithoutFeedingReceiver();
}

SourceLoc Parser::getEndOfPreviousLoc() const {
  return Lexer::getLocForEndOfToken(SourceMgr, PreviousLoc);
}

SourceLoc Parser::consumeStartingCharacterOfCurrentToken(tok Kind, size_t Len) {
  // Consumes prefix of token and returns its location.
  // (like '?', '<', '>' or '!' immediately followed by '<') 
  assert(Len >= 1);

  // Current token can be either one-character token we want to consume...
  if (Tok.getLength() == Len) {
    Tok.setKind(Kind);
    return consumeToken();
  }

  auto Loc = Tok.getLoc();

  // ... or a multi-character token with the first N characters being the one
  // that we want to consume as a separate token.
  assert(Tok.getLength() > Len);
  markSplitToken(Kind, Tok.getText().substr(0, Len));

  auto NewState = L->getStateForBeginningOfTokenLoc(Loc.getAdvancedLoc(Len));
  restoreParserPosition(ParserPosition(NewState, Loc),
                        /*enableDiagnostics=*/true);
  return PreviousLoc;
}

void Parser::markSplitToken(tok Kind, StringRef Txt) {
  SplitTokens.emplace_back();
  SplitTokens.back().setToken(Kind, Txt);
  ParsedTrivia EmptyTrivia;
  SyntaxContext->addToken(SplitTokens.back(), LeadingTrivia, EmptyTrivia);
  TokReceiver->receive(SplitTokens.back());
}

SourceLoc Parser::consumeStartingLess() {
  assert(startsWithLess(Tok) && "Token does not start with '<'");
  return consumeStartingCharacterOfCurrentToken(tok::l_angle);
}

SourceLoc Parser::consumeStartingGreater() {
  assert(startsWithGreater(Tok) && "Token does not start with '>'");
  return consumeStartingCharacterOfCurrentToken(tok::r_angle);
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
  case tok::pound_if:
  case tok::pound_else:
  case tok::pound_elseif:
    consumeToken();
    // skipUntil also implicitly stops at tok::pound_endif.
    skipUntil(tok::pound_else, tok::pound_elseif);
      
    if (Tok.isAny(tok::pound_else, tok::pound_elseif))
      skipSingle();
    else
      consumeIf(tok::pound_endif);
    break;
      
  default:
    consumeToken();
    break;
  }
}

void Parser::skipUntil(tok T1, tok T2) {
  // tok::NUM_TOKENS is a sentinel that means "don't skip".
  if (T1 == tok::NUM_TOKENS && T2 == tok::NUM_TOKENS) return;
  
  while (Tok.isNot(T1, T2, tok::eof, tok::pound_endif, tok::code_complete))
    skipSingle();
}

void Parser::skipUntilAnyOperator() {
  while (Tok.isNot(tok::eof, tok::pound_endif, tok::code_complete) &&
         Tok.isNotAnyOperator())
    skipSingle();
}

/// Skip until a token that starts with '>', and consume it if found.
/// Applies heuristics that are suitable when trying to find the end of a list
/// of generic parameters, generic arguments, or list of types in a protocol
/// composition.
SourceLoc Parser::skipUntilGreaterInTypeList(bool protocolComposition) {
  SourceLoc lastLoc = PreviousLoc;
  while (true) {
    switch (Tok.getKind()) {
    case tok::eof:
    case tok::l_brace:
    case tok::r_brace:
    case tok::code_complete:
      return lastLoc;

#define KEYWORD(X) case tok::kw_##X:
#define POUND_KEYWORD(X) case tok::pound_##X:
#include "swift/Syntax/TokenKinds.def"
    // 'Self' can appear in types, skip it.
    if (Tok.is(tok::kw_Self))
      break;
    if (isStartOfStmt() || isStartOfDecl() || Tok.is(tok::pound_endif))
      return lastLoc;
    break;

    case tok::l_paren:
    case tok::r_paren:
    case tok::l_square:
    case tok::r_square:
      // In generic type parameter list, skip '[' ']' '(' ')', because they
      // can appear in types.
      if (protocolComposition)
        return lastLoc;
      break;

    default:
      if (Tok.isAnyOperator() && startsWithGreater(Tok))
        return consumeStartingGreater();
      
      break;
    }
    skipSingle();
    lastLoc = PreviousLoc;
  }
}

void Parser::skipUntilDeclRBrace() {
  while (Tok.isNot(tok::eof, tok::r_brace, tok::pound_endif,
                   tok::pound_else, tok::pound_elseif,
                   tok::code_complete) &&
         !isStartOfDecl())
    skipSingle();
}

void Parser::skipUntilDeclStmtRBrace(tok T1) {
  while (Tok.isNot(T1, tok::eof, tok::r_brace, tok::pound_endif,
                   tok::pound_else, tok::pound_elseif,
                   tok::code_complete) &&
         !isStartOfStmt() && !isStartOfDecl()) {
    skipSingle();
  }
}

void Parser::skipUntilDeclStmtRBrace(tok T1, tok T2) {
  while (Tok.isNot(T1, T2, tok::eof, tok::r_brace, tok::pound_endif,
                   tok::pound_else, tok::pound_elseif,
                   tok::code_complete) &&
         !isStartOfStmt() && !isStartOfDecl()) {
    skipSingle();
  }
}

void Parser::skipListUntilDeclRBrace(SourceLoc startLoc, tok T1, tok T2) {
  while (Tok.isNot(T1, T2, tok::eof, tok::r_brace, tok::pound_endif,
                   tok::pound_else, tok::pound_elseif)) {
    bool hasDelimiter = Tok.getLoc() == startLoc || consumeIf(tok::comma);
    bool possibleDeclStartsLine = Tok.isAtStartOfLine();
    
    if (isStartOfDecl()) {
      
      // Could have encountered something like `_ var:` 
      // or `let foo:` or `var:`
      if (Tok.isAny(tok::kw_var, tok::kw_let)) {
        if (possibleDeclStartsLine && !hasDelimiter) {
          break;
        }
        
        Parser::BacktrackingScope backtrack(*this);
        // Consume the let or var
        consumeToken();
        
        // If the following token is either <identifier> or :, it means that
        // this `var` or `let` shoud be interpreted as a label
        if ((Tok.canBeArgumentLabel() && peekToken().is(tok::colon)) ||
             peekToken().is(tok::colon)) {
          backtrack.cancelBacktrack();
          continue;
        }
      }
      break;
    }
    skipSingle();
  }
}

void Parser::skipUntilDeclRBrace(tok T1, tok T2) {
  while (Tok.isNot(T1, T2, tok::eof, tok::r_brace, tok::pound_endif,
                   tok::pound_else, tok::pound_elseif) &&
         !isStartOfDecl()) {
    skipSingle();
  }
}

void Parser::skipUntilConditionalBlockClose() {
  while (Tok.isNot(tok::pound_else, tok::pound_elseif, tok::pound_endif,
                   tok::eof)) {
    skipSingle();
  }
}

bool Parser::skipUntilTokenOrEndOfLine(tok T1) {
  while (Tok.isNot(tok::eof, T1) && !Tok.isAtStartOfLine())
    skipSingle();

  return Tok.is(T1) && !Tok.isAtStartOfLine();
}

bool Parser::loadCurrentSyntaxNodeFromCache() {
  // Don't do a cache lookup when not building a syntax tree since otherwise
  // the corresponding AST nodes do not get created
  if (!SF.shouldBuildSyntaxTree()) {
    return false;
  }
  unsigned LexerOffset =
      SourceMgr.getLocOffsetInBuffer(Tok.getLoc(), L->getBufferID());
  unsigned LeadingTriviaLen = LeadingTrivia.getLength();
  unsigned LeadingTriviaOffset = LexerOffset - LeadingTriviaLen;
  SourceLoc LeadingTriviaLoc = Tok.getLoc().getAdvancedLoc(-LeadingTriviaLen);
  if (auto TextLength = SyntaxContext->lookupNode(LeadingTriviaOffset,
                                                  LeadingTriviaLoc)) {
    L->resetToOffset(LeadingTriviaOffset + TextLength);
    L->lex(Tok, LeadingTrivia, TrailingTrivia);
    return true;
  }
  return false;
}

bool Parser::parseEndIfDirective(SourceLoc &Loc) {
  Loc = Tok.getLoc();
  if (parseToken(tok::pound_endif, diag::expected_close_to_if_directive)) {
    Loc = PreviousLoc;
    skipUntilConditionalBlockClose();
    return true;
  } else if (!Tok.isAtStartOfLine() && Tok.isNot(tok::eof))
    diagnose(Tok.getLoc(),
             diag::extra_tokens_conditional_compilation_directive);
  return false;
}

static Parser::StructureMarkerKind
getStructureMarkerKindForToken(const Token &tok) {
  switch (tok.getKind()) {
  case tok::l_brace:
    return Parser::StructureMarkerKind::OpenBrace;
  case tok::l_paren:
    return Parser::StructureMarkerKind::OpenParen;
  case tok::l_square:
    return Parser::StructureMarkerKind::OpenSquare;
  default:
    llvm_unreachable("Not a matched token");
  }
}

Parser::StructureMarkerRAII::StructureMarkerRAII(Parser &parser,
                                                 const Token &tok)
    : StructureMarkerRAII(parser, tok.getLoc(),
                          getStructureMarkerKindForToken(tok)) {}

bool Parser::StructureMarkerRAII::pushStructureMarker(
                                      Parser &parser, SourceLoc loc,    
                                      StructureMarkerKind kind) {
  
  if (parser.StructureMarkers.size() < MaxDepth) {
    parser.StructureMarkers.push_back({loc, kind, None});
    return true;
  } else {
    parser.diagnose(loc, diag::structure_overflow, MaxDepth);
    // We need to cut off parsing or we will stack-overflow.
    // But `cutOffParsing` changes the current token to eof, and we may be in
    // a place where `consumeToken()` will be expecting e.g. '[',
    // since we need that to get to the callsite, so this can cause an assert.
    parser.cutOffParsing();
    return false;
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
  case tok::identifier:
    Loc = consumeIdentifier(&Result);
    return false;
  default:
    checkForInputIncomplete();
    diagnose(Tok, D);
    return true;
  }
}

bool Parser::parseSpecificIdentifier(StringRef expected, SourceLoc &loc,
                                     const Diagnostic &D) {
  if (Tok.getText() != expected) {
    diagnose(Tok, D);
    return true;
  }
  loc = consumeToken(tok::identifier);
  return false;
}

/// parseAnyIdentifier - Consume an identifier or operator if present and return
/// its name in Result.  Otherwise, emit an error and return true.
bool Parser::parseAnyIdentifier(Identifier &Result, SourceLoc &Loc,
                                const Diagnostic &D) {
  if (Tok.is(tok::identifier)) {
    Loc = consumeIdentifier(&Result);
    return false;
  }

  if (Tok.isAnyOperator()) {
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

  if (Tok.isKeyword()) {
    diagnose(Tok, diag::keyword_cant_be_identifier, Tok.getText());
    diagnose(Tok, diag::backticks_to_escape)
      .fixItReplace(Tok.getLoc(), "`" + Tok.getText().str() + "`");
  } else {
    diagnose(Tok, D);
  }

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

    TokLoc = getLocForMissingMatchingToken();
    return true;
  }

  return false;
}

bool Parser::parseUnsignedInteger(unsigned &Result, SourceLoc &Loc,
                                  const Diagnostic &D) {
  auto IntTok = Tok;
  if (parseToken(tok::integer_literal, Loc, D))
    return true;
  if (IntTok.getText().getAsInteger(0, Result)) {
    diagnose(IntTok.getLoc(), D);
    return true;
  }
  return false;
}

SourceLoc Parser::getLocForMissingMatchingToken() const {
  // At present, use the same location whether it's an error or whether
  // the matching token is missing.
  // Both cases supply a location for something the user didn't type.
  return getErrorOrMissingLoc();
}

SourceLoc Parser::getErrorOrMissingLoc() const {
  // The next token might start a new enclosing construct,
  // and SourceLoc's are always at the start of a token (for example, for
  // fixits, so use the previous token's SourceLoc and allow a subnode to end
  // right at the same place as its supernode.

  // The tricky case is when the previous token is an InterpolatedStringLiteral.
  // Then, there will be names in scope whose SourceLoc is *after* the
  // the location of a missing close brace.
  // ASTScope tree creation will have to cope.

  return PreviousLoc;
}

static SyntaxKind getListElementKind(SyntaxKind ListKind) {
  switch (ListKind) {
  case SyntaxKind::TupleExprElementList:
    return SyntaxKind::TupleExprElement;
  case SyntaxKind::ArrayElementList:
    return SyntaxKind::ArrayElement;
  case SyntaxKind::DictionaryElementList:
    return SyntaxKind::DictionaryElement;
  case SyntaxKind::FunctionParameterList:
    return SyntaxKind::FunctionParameter;
  case SyntaxKind::TupleTypeElementList:
    return SyntaxKind::TupleTypeElement;
  case SyntaxKind::TuplePatternElementList:
    return SyntaxKind::TuplePatternElement;
  default:
    return SyntaxKind::Unknown;
  }
}

ParserStatus
Parser::parseList(tok RightK, SourceLoc LeftLoc, SourceLoc &RightLoc,
                  bool AllowSepAfterLast, Diag<> ErrorDiag, SyntaxKind Kind,
                  llvm::function_ref<ParserStatus()> callback) {
  auto TokIsStringInterpolationEOF = [&]() -> bool {
    return Tok.is(tok::eof) && Tok.getText() == ")" && RightK == tok::r_paren;
  };
  
  llvm::Optional<SyntaxParsingContext> ListContext;
  ListContext.emplace(SyntaxContext, Kind);
  if (Kind == SyntaxKind::Unknown)
    ListContext->setTransparent();

  SyntaxKind ElementKind = getListElementKind(Kind);

  if (Tok.is(RightK)) {
    ListContext.reset();
    RightLoc = consumeToken(RightK);
    return makeParserSuccess();
  }
  if (TokIsStringInterpolationEOF()) {
    RightLoc = Tok.getLoc();
    return makeParserSuccess();
  }

  ParserStatus Status;
  while (true) {
    while (Tok.is(tok::comma)) {
      diagnose(Tok, diag::unexpected_separator, ",")
        .fixItRemove(SourceRange(Tok.getLoc()));
      consumeToken();
    }
    SourceLoc StartLoc = Tok.getLoc();

    SyntaxParsingContext ElementContext(SyntaxContext, ElementKind);
    if (ElementKind == SyntaxKind::Unknown)
      ElementContext.setTransparent();
    Status |= callback();
    if (Tok.is(RightK))
      break;
    // If the lexer stopped with an EOF token whose spelling is ")", then this
    // is actually the tuple that is a string literal interpolation context.
    // Just accept the ")" and build the tuple as we usually do.
    if (TokIsStringInterpolationEOF()) {
      RightLoc = Tok.getLoc();
      return Status;
    }
    // If we haven't made progress, or seeing any error, skip ahead.
    if (Tok.getLoc() == StartLoc || Status.isError()) {
      assert(Status.isError() && "no progress without error");
      skipListUntilDeclRBrace(LeftLoc, RightK, tok::comma);
      if (Tok.is(RightK) || Tok.isNot(tok::comma))
        break;
    }
    if (consumeIf(tok::comma)) {
      if (Tok.isNot(RightK))
        continue;
      if (!AllowSepAfterLast) {
        diagnose(Tok, diag::unexpected_separator, ",")
          .fixItRemove(SourceRange(PreviousLoc));
      }
      break;
    }
    // If we're in a comma-separated list, the next token is at the
    // beginning of a new line and can never start an element, break.
    if (Tok.isAtStartOfLine() &&
        (Tok.is(tok::r_brace) || isStartOfDecl() || isStartOfStmt())) {
      break;
    }
    // If we found EOF or such, bailout.
    if (Tok.isAny(tok::eof, tok::pound_endif)) {
      IsInputIncomplete = true;
      break;
    }

    diagnose(Tok, diag::expected_separator, ",")
      .fixItInsertAfter(PreviousLoc, ",");
    Status.setIsParseError();
  }

  ListContext.reset();

  if (Status.isError()) {
    // If we've already got errors, don't emit missing RightK diagnostics.
    RightLoc =
        Tok.is(RightK) ? consumeToken() : getLocForMissingMatchingToken();
  } else if (parseMatchingToken(RightK, RightLoc, ErrorDiag, LeftLoc)) {
    Status.setIsParseError();
  }

  return Status;
}

/// diagnoseRedefinition - Diagnose a redefinition error, with a note
/// referring back to the original definition.

void Parser::diagnoseRedefinition(ValueDecl *Prev, ValueDecl *New) {
  assert(New != Prev && "Cannot conflict with self");
  diagnose(New->getLoc(), diag::decl_redefinition);
  diagnose(Prev->getLoc(), diag::previous_decldef, Prev->getBaseName());
}

Optional<StringRef>
Parser::getStringLiteralIfNotInterpolated(SourceLoc Loc,
                                          StringRef DiagText) {
  assert(Tok.is(tok::string_literal));

  // FIXME: Support extended escaping string literal.
  if (Tok.getCustomDelimiterLen()) {
    diagnose(Loc, diag::forbidden_extended_escaping_string, DiagText);
    return None;
  }

  SmallVector<Lexer::StringSegment, 1> Segments;
  L->getStringLiteralSegments(Tok, Segments);
  if (Segments.size() != 1 ||
      Segments.front().Kind == Lexer::StringSegment::Expr) {
    diagnose(Loc, diag::forbidden_interpolated_string, DiagText);
    return None;
  }

  return SourceMgr.extractText(CharSourceRange(Segments.front().Loc,
                                               Segments.front().Length));
}

struct ParserUnit::Implementation {
  std::shared_ptr<SyntaxParseActions> SPActions;
  LangOptions LangOpts;
  TypeCheckerOptions TypeCheckerOpts;
  SearchPathOptions SearchPathOpts;
  DiagnosticEngine Diags;
  ASTContext &Ctx;
  SourceFile *SF;
  std::unique_ptr<Parser> TheParser;

  Implementation(SourceManager &SM, SourceFileKind SFKind, unsigned BufferID,
                 const LangOptions &Opts, const TypeCheckerOptions &TyOpts,
                 StringRef ModuleName,
                 std::shared_ptr<SyntaxParseActions> spActions)
      : SPActions(std::move(spActions)),
        LangOpts(Opts), TypeCheckerOpts(TyOpts), Diags(SM),
        Ctx(*ASTContext::get(LangOpts, TypeCheckerOpts, SearchPathOpts, SM, Diags)),
        SF(new (Ctx) SourceFile(
            *ModuleDecl::create(Ctx.getIdentifier(ModuleName), Ctx), SFKind,
            BufferID, SourceFile::ImplicitModuleImportKind::None,
            Opts.CollectParsedToken, Opts.BuildSyntaxTree)) {}

  ~Implementation() {
    // We need to delete the parser before the context so that it can finalize
    // its SourceFileSyntax while it is still alive
    TheParser.reset();
    delete &Ctx;
  }
};

ParserUnit::ParserUnit(SourceManager &SM, SourceFileKind SFKind, unsigned BufferID)
  : ParserUnit(SM, SFKind, BufferID,
               LangOptions(), TypeCheckerOptions(), "input") {
}

ParserUnit::ParserUnit(SourceManager &SM, SourceFileKind SFKind, unsigned BufferID,
                       const LangOptions &LangOpts,
                       const TypeCheckerOptions &TypeCheckOpts,
                       StringRef ModuleName,
                       std::shared_ptr<SyntaxParseActions> spActions,
                       SyntaxParsingCache *SyntaxCache)
    : Impl(*new Implementation(SM, SFKind, BufferID, LangOpts, TypeCheckOpts,
                               ModuleName, std::move(spActions))) {

  Impl.SF->SyntaxParsingCache = SyntaxCache;
  Impl.TheParser.reset(new Parser(BufferID, *Impl.SF, /*SIL=*/nullptr,
                                  /*PersistentState=*/nullptr, Impl.SPActions,
                                  /*DelayBodyParsing=*/false));
}

ParserUnit::ParserUnit(SourceManager &SM, SourceFileKind SFKind, unsigned BufferID,
                       unsigned Offset, unsigned EndOffset)
  : Impl(*new Implementation(SM, SFKind, BufferID, LangOptions(),
                             TypeCheckerOptions(), "input", nullptr)) {

  std::unique_ptr<Lexer> Lex;
  Lex.reset(new Lexer(Impl.LangOpts, SM,
                      BufferID, &Impl.Diags,
                      LexerMode::Swift,
                      HashbangMode::Allowed,
                      CommentRetentionMode::None,
                      TriviaRetentionMode::WithoutTrivia,
                      Offset, EndOffset));
  Impl.TheParser.reset(new Parser(std::move(Lex), *Impl.SF, /*SIL=*/nullptr,
    /*PersistentState=*/nullptr, Impl.SPActions, /*DelayBodyParsing=*/false));
}

ParserUnit::~ParserUnit() {
  delete &Impl;
}

OpaqueSyntaxNode ParserUnit::parse() {
  auto &P = getParser();
  bool Done = false;
  while (!Done) {
    P.parseTopLevel();
    Done = P.Tok.is(tok::eof);
  }
  return P.finalizeSyntaxTree();
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

ParsedDeclName swift::parseDeclName(StringRef name) {
  if (name.empty()) return ParsedDeclName();

  // Local function to handle the parsing of the base name + context.
  //
  // Returns true if an error occurred, without recording the base name.
  ParsedDeclName result;
  auto parseBaseName = [&](StringRef text) -> bool {
    // Split the text into context name and base name.
    StringRef contextName, baseName;
    std::tie(contextName, baseName) = text.rsplit('.');
    if (baseName.empty()) {
      baseName = contextName;
      contextName = StringRef();
    } else if (contextName.empty()) {
      return true;
    }

    auto isValidIdentifier = [](StringRef text) -> bool {
      return Lexer::isIdentifier(text) && text != "_";
    };

    // Make sure we have an identifier for the base name.
    if (!isValidIdentifier(baseName))
      return true;

    // If we have a context, make sure it is an identifier, or a series of
    // dot-separated identifiers.
    // FIXME: What about generic parameters?
    if (!contextName.empty()) {
      StringRef first;
      StringRef rest = contextName;
      do {
        std::tie(first, rest) = rest.split('.');
        if (!isValidIdentifier(first))
          return true;
      } while (!rest.empty());
    }

    // Record the results.
    result.ContextName = contextName;
    result.BaseName = baseName;
    return false;
  };

  // If this is not a function name, just parse the base name and
  // we're done.
  if (name.back() != ')') {
    if (Lexer::isOperator(name))
      result.BaseName = name;
    else if (parseBaseName(name))
      return ParsedDeclName();
    return result;
  }

  // We have a function name.
  result.IsFunctionName = true;

  // Split the base name from the parameters.
  StringRef baseName, parameters;
  std::tie(baseName, parameters) = name.split('(');
  if (parameters.empty()) return ParsedDeclName();

  // If the base name is prefixed by "getter:" or "setter:", it's an
  // accessor.
  if (baseName.startswith("getter:")) {
    result.IsGetter = true;
    result.IsFunctionName = false;
    baseName = baseName.substr(7);
  } else if (baseName.startswith("setter:")) {
    result.IsSetter = true;
    result.IsFunctionName = false;
    baseName = baseName.substr(7);
  }

  // Parse the base name.
  if (parseBaseName(baseName)) return ParsedDeclName();

  parameters = parameters.drop_back(); // ')'
  if (parameters.empty()) return result;

  if (parameters.back() != ':')
    return ParsedDeclName();

  bool isMember = !result.ContextName.empty();
  do {
    StringRef NextParam;
    std::tie(NextParam, parameters) = parameters.split(':');

    if (!Lexer::isIdentifier(NextParam))
      return ParsedDeclName();
    if (NextParam == "_") {
      result.ArgumentLabels.push_back("");
    } else if (isMember && NextParam == "self") {
      // For a member, "self" indicates the self parameter. There can
      // only be one such parameter.
      if (result.SelfIndex) return ParsedDeclName();
      result.SelfIndex = result.ArgumentLabels.size();
    } else {
      result.ArgumentLabels.push_back(NextParam);
    }
  } while (!parameters.empty());

  // Drop the argument labels for a property accessor; they aren't used.
  if (result.isPropertyAccessor())
    result.ArgumentLabels.clear();

  return result;
}

DeclName ParsedDeclName::formDeclName(ASTContext &ctx) const {
  return swift::formDeclName(ctx, BaseName, ArgumentLabels, IsFunctionName,
                             /*IsInitializer=*/true);
}

DeclName swift::formDeclName(ASTContext &ctx,
                             StringRef baseName,
                             ArrayRef<StringRef> argumentLabels,
                             bool isFunctionName,
                             bool isInitializer) {
  // We cannot import when the base name is not an identifier.
  if (baseName.empty())
    return DeclName();
  if (!Lexer::isIdentifier(baseName) && !Lexer::isOperator(baseName))
    return DeclName();

  // Get the identifier for the base name. Special-case `init`.
  DeclBaseName baseNameId = ((isInitializer && baseName == "init")
                             ? DeclBaseName::createConstructor()
                             : ctx.getIdentifier(baseName));

  // For non-functions, just use the base name.
  if (!isFunctionName) return baseNameId;

  // For functions, we need to form a complete name.

  // Convert the argument names.
  SmallVector<Identifier, 4> argumentLabelIds;
  for (auto argName : argumentLabels) {
    if (argumentLabels.empty() || !Lexer::isIdentifier(argName)) {
      argumentLabelIds.push_back(Identifier());
      continue;
    }

    argumentLabelIds.push_back(ctx.getIdentifier(argName));
  }

  // Build the result.
  return DeclName(ctx, baseNameId, argumentLabelIds);
}

DeclName swift::parseDeclName(ASTContext &ctx, StringRef name) {
  return parseDeclName(name).formDeclName(ctx);
}
