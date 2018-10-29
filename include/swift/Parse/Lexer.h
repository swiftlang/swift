//===--- Lexer.h - Swift Language Lexer -------------------------*- C++ -*-===//
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
//  This file defines the Lexer interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LEXER_H
#define SWIFT_LEXER_H

#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/LexerState.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/Trivia.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/SaveAndRestore.h"

namespace swift {

  /// Given a pointer to the starting byte of a UTF8 character, validate it and
  /// advance the lexer past it.  This returns the encoded character or ~0U if
  /// the encoding is invalid.
  uint32_t validateUTF8CharacterAndAdvance(const char *&Ptr, const char *End);

  class DiagnosticEngine;
  class InFlightDiagnostic;
  class LangOptions;

  template<typename ...T> struct Diag;

enum class CommentRetentionMode {
  None,
  AttachToNextToken,
  ReturnAsTokens,
};

enum class TriviaRetentionMode {
  WithoutTrivia,
  WithTrivia,
};

enum class HashbangMode : bool {
  Disallowed,
  Allowed,
};

/// Kinds of conflict marker which the lexer might encounter.
enum class ConflictMarkerKind {
  /// A normal or diff3 conflict marker, initiated by at least 7 "<"s,
  /// separated by at least 7 "="s or "|"s, and terminated by at least 7 ">"s.
  Normal,
  /// A Perforce-style conflict marker, initiated by 4 ">"s,
  /// separated by 4 "="s, and terminated by 4 "<"s.
  Perforce
};

class Lexer {
  const LangOptions &LangOpts;
  const SourceManager &SourceMgr;
  const unsigned BufferID;
  DiagnosticEngine *Diags;

  using State = LexerState;

  /// Pointer to the first character of the buffer, even in a lexer that
  /// scans a subrange of the buffer.
  const char *BufferStart;

  /// Pointer to one past the end character of the buffer, even in a lexer
  /// that scans a subrange of the buffer.  Because the buffer is always
  /// NUL-terminated, this points to the NUL terminator.
  const char *BufferEnd;

  /// Pointer to the artificial EOF that is located before BufferEnd.  Useful
  /// for lexing subranges of a buffer.
  const char *ArtificialEOF = nullptr;

  /// If non-null, points to the '\0' character in the buffer where we should
  /// produce a code completion token.
  const char *CodeCompletionPtr = nullptr;

  /// Points to BufferStart or past the end of UTF-8 BOM sequence if it exists.
  const char *ContentStart;

  /// Pointer to the next not consumed character.
  const char *CurPtr;

  Token NextToken;
  
  /// \brief This is true if we're lexing a .sil file instead of a .swift
  /// file.  This enables the 'sil' keyword.
  const bool InSILMode;

  /// True if we should skip past a `#!` line at the start of the file.
  const bool IsHashbangAllowed;

  const CommentRetentionMode RetainComments;

  const TriviaRetentionMode TriviaRetention;

  /// InSILBody - This is true when we're lexing the body of a SIL declaration
  /// in a SIL file.  This enables some context-sensitive lexing.
  bool InSILBody = false;

  /// The current leading trivia for the next token.
  ///
  /// This is only preserved if this Lexer was constructed with
  /// `TriviaRetentionMode::WithTrivia`.
  syntax::Trivia LeadingTrivia;

  /// The current trailing trivia for the next token.
  ///
  /// This is only preserved if this Lexer was constructed with
  /// `TriviaRetentionMode::WithTrivia`.
  syntax::Trivia TrailingTrivia;
  
  Lexer(const Lexer&) = delete;
  void operator=(const Lexer&) = delete;

  struct PrincipalTag {};

  /// The principal constructor used by public constructors below.
  /// Don't use this constructor for other purposes, it does not initialize
  /// everything.
  Lexer(const PrincipalTag &, const LangOptions &LangOpts,
        const SourceManager &SourceMgr, unsigned BufferID,
        DiagnosticEngine *Diags, bool InSILMode, HashbangMode HashbangAllowed,
        CommentRetentionMode RetainComments,
        TriviaRetentionMode TriviaRetention);

  void initialize(unsigned Offset, unsigned EndOffset);

public:
  /// \brief Create a normal lexer that scans the whole source buffer.
  ///
  /// \param Options - the language options under which to lex.  By
  ///   design, language options only affect whether a token is valid
  ///   and/or the exact token kind produced (e.g. keyword or
  ///   identifier), but not things like how many characters are
  ///   consumed.  If that changes, APIs like getLocForEndOfToken will
  ///   need to take a LangOptions explicitly.
  /// \param InSILMode - whether we're parsing a SIL source file.
  ///   Unlike language options, this does affect primitive lexing, which
  ///   means that APIs like getLocForEndOfToken really ought to take
  ///   this flag; it's just that we don't care that much about fidelity
  ///   when parsing SIL files.
  Lexer(
      const LangOptions &Options, const SourceManager &SourceMgr,
      unsigned BufferID, DiagnosticEngine *Diags, bool InSILMode,
      HashbangMode HashbangAllowed = HashbangMode::Disallowed,
      CommentRetentionMode RetainComments = CommentRetentionMode::None,
      TriviaRetentionMode TriviaRetention = TriviaRetentionMode::WithoutTrivia);

  /// \brief Create a lexer that scans a subrange of the source buffer.
  Lexer(const LangOptions &Options, const SourceManager &SourceMgr,
        unsigned BufferID, DiagnosticEngine *Diags, bool InSILMode,
        HashbangMode HashbangAllowed, CommentRetentionMode RetainComments,
        TriviaRetentionMode TriviaRetention, unsigned Offset,
        unsigned EndOffset);

  /// \brief Create a sub-lexer that lexes from the same buffer, but scans
  /// a subrange of the buffer.
  ///
  /// \param Parent the parent lexer that scans the whole buffer
  /// \param BeginState start of the subrange
  /// \param EndState end of the subrange
  Lexer(Lexer &Parent, State BeginState, State EndState);

  /// \brief Returns true if this lexer will produce a code completion token.
  bool isCodeCompletion() const {
    return CodeCompletionPtr != nullptr;
  }

  /// Lex a token. If \c TriviaRetentionMode is \c WithTrivia, passed pointers
  /// to trivias are populated.
  void lex(Token &Result, syntax::Trivia &LeadingTriviaResult,
           syntax::Trivia &TrailingTriviaResult) {
    Result = NextToken;
    if (TriviaRetention == TriviaRetentionMode::WithTrivia) {
      LeadingTriviaResult = {LeadingTrivia};
      TrailingTriviaResult = {TrailingTrivia};
    }
    if (Result.isNot(tok::eof))
      lexImpl();
  }

  void lex(Token &Result) {
    syntax::Trivia LeadingTrivia, TrailingTrivia;
    lex(Result, LeadingTrivia, TrailingTrivia);
  }

  /// Reset the lexer's buffer pointer to \p Offset bytes after the buffer
  /// start.
  void resetToOffset(size_t Offset) {
    assert(BufferStart + Offset <= BufferEnd && "Offset after buffer end");

    CurPtr = BufferStart + Offset;
    lexImpl();
  }

  bool isKeepingComments() const {
    return RetainComments == CommentRetentionMode::ReturnAsTokens;
  }

  unsigned getBufferID() const { return BufferID; }

  /// peekNextToken - Return the next token to be returned by Lex without
  /// actually lexing it.
  const Token &peekNextToken() const { return NextToken; }

  /// \brief Returns the lexer state for the beginning of the given token
  /// location. After restoring the state, lexer will return this token and
  /// continue from there.
  State getStateForBeginningOfTokenLoc(SourceLoc Loc) const;

  /// \brief Returns the lexer state for the beginning of the given token.
  /// After restoring the state, lexer will return this token and continue from
  /// there.
  State getStateForBeginningOfToken(const Token &Tok,
                                    const syntax::Trivia &LeadingTrivia = {}) const {

    // If the token has a comment attached to it, rewind to before the comment,
    // not just the start of the token.  This ensures that we will re-lex and
    // reattach the comment to the token if rewound to this state.
    SourceLoc TokStart = Tok.getCommentStart();
    if (TokStart.isInvalid())
      TokStart = Tok.getLoc();
    auto S = getStateForBeginningOfTokenLoc(TokStart);
    if (TriviaRetention == TriviaRetentionMode::WithTrivia)
      S.LeadingTrivia = LeadingTrivia;
    return S;
  }

  State getStateForEndOfTokenLoc(SourceLoc Loc) const {
    return State(getLocForEndOfToken(SourceMgr, Loc));
  }

  bool isStateForCurrentBuffer(LexerState State) const {
    return SourceMgr.findBufferContainingLoc(State.Loc) == getBufferID();
  }

  /// \brief Restore the lexer state to a given one, that can be located either
  /// before or after the current position.
  void restoreState(State S, bool enableDiagnostics = false) {
    assert(S.isValid());
    CurPtr = getBufferPtrForSourceLoc(S.Loc);
    // Don't reemit diagnostics while readvancing the lexer.
    llvm::SaveAndRestore<DiagnosticEngine*>
      D(Diags, enableDiagnostics ? Diags : nullptr);

    lexImpl();

    // Restore Trivia.
    if (TriviaRetention == TriviaRetentionMode::WithTrivia)
      if (auto &LTrivia = S.LeadingTrivia)
        LeadingTrivia = std::move(*LTrivia);
  }

  /// \brief Restore the lexer state to a given state that is located before
  /// current position.
  void backtrackToState(State S) {
    assert(getBufferPtrForSourceLoc(S.Loc) <= CurPtr &&
           "can't backtrack forward");
    restoreState(S);
  }

  /// \brief Retrieve the Token referred to by \c Loc.
  ///
  /// \param SM The source manager in which the given source location
  /// resides.
  ///
  /// \param Loc The source location of the beginning of a token.
  static Token getTokenAtLocation(const SourceManager &SM, SourceLoc Loc);


  /// \brief Retrieve the source location that points just past the
  /// end of the token referred to by \c Loc.
  ///
  /// \param SM The source manager in which the given source location
  /// resides.
  ///
  /// \param Loc The source location of the beginning of a token.
  static SourceLoc getLocForEndOfToken(const SourceManager &SM, SourceLoc Loc);

  /// \brief Convert a SourceRange to the equivalent CharSourceRange
  ///
  /// \param SM The source manager in which the given source range
  /// resides.
  ///
  /// \param SR The source range
  static CharSourceRange
  getCharSourceRangeFromSourceRange(const SourceManager &SM,
                                    const SourceRange &SR) {
    return CharSourceRange(SM, SR.Start, getLocForEndOfToken(SM, SR.End));
  }

  /// Return the start location of the token that the offset in the given buffer
  /// points to.
  ///
  /// Note that this is more expensive than \c getLocForEndOfToken because it
  /// finds and re-lexes from the beginning of the line.
  ///
  /// Due to the parser splitting tokens the adjustment may be incorrect, e.g:
  /// \code
  ///   func +<T>(a : T, b : T)
  /// \endcode
  /// The start of the '<' token is '<', but the lexer will produce "+<" before
  /// the parser splits it up.
  ////
  /// If the offset points to whitespace the returned source location will point
  /// to the whitespace offset.
  static SourceLoc getLocForStartOfToken(SourceManager &SM, unsigned BufferID,
                                         unsigned Offset);

  static SourceLoc getLocForStartOfToken(SourceManager &SM, SourceLoc Loc);

  /// Retrieve the start location of the line containing the given location.
  /// the given location.
  static SourceLoc getLocForStartOfLine(SourceManager &SM, SourceLoc Loc);

  /// Retrieve the source location for the end of the line containing the
  /// given token, which is the location of the start of the next line.
  static SourceLoc getLocForEndOfLine(SourceManager &SM, SourceLoc Loc);

  /// Retrieve the string used to indent the line that contains the given
  /// source location.
  ///
  /// If \c ExtraIndentation is not null, it will be set to an appropriate
  /// additional intendation for adding code in a smaller scope "within" \c Loc.
  static StringRef getIndentationForLine(SourceManager &SM, SourceLoc Loc,
                                         StringRef *ExtraIndentation = nullptr);

  /// \brief Determines if the given string is a valid non-operator
  /// identifier, without escaping characters.
  static bool isIdentifier(StringRef identifier);

  /// \brief Determine the token kind of the string, given that it is a valid
  /// non-operator identifier. Return tok::identifier if the string is not a
  /// reserved word.
  static tok kindOfIdentifier(StringRef Str, bool InSILMode);

  /// \brief Determines if the given string is a valid operator identifier,
  /// without escaping characters.
  static bool isOperator(StringRef string);

  SourceLoc getLocForStartOfBuffer() const {
    return SourceLoc(llvm::SMLoc::getFromPointer(BufferStart));
  }
  
  /// StringSegment - A segment of a (potentially interpolated) string.
  struct StringSegment {
    enum : char { Literal, Expr } Kind;
    // Loc+Length for the segment inside the string literal, without quotes.
    SourceLoc Loc;
    unsigned Length, IndentToStrip, CustomDelimiterLen;
    bool IsFirstSegment, IsLastSegment;

    static StringSegment getLiteral(SourceLoc Loc, unsigned Length,
                                    bool IsFirstSegment, bool IsLastSegment,
                                    unsigned IndentToStrip,
                                    unsigned CustomDelimiterLen) {
      StringSegment Result;
      Result.Kind = Literal;
      Result.Loc = Loc;
      Result.Length = Length;
      Result.IsFirstSegment = IsFirstSegment;
      Result.IsLastSegment = IsLastSegment;
      Result.IndentToStrip = IndentToStrip;
      Result.CustomDelimiterLen = CustomDelimiterLen;
      return Result;
    }
    
    static StringSegment getExpr(SourceLoc Loc, unsigned Length) {
      StringSegment Result;
      Result.Kind = Expr;
      Result.Loc = Loc;
      Result.Length = Length;
      Result.IsFirstSegment = false;
      Result.IsLastSegment = false;
      Result.IndentToStrip = 0;
      Result.CustomDelimiterLen = 0;
      return Result;
    }

    SourceLoc getEndLoc() {
      return Loc.getAdvancedLoc(Length);
    }

  };

  /// Implementation of getEncodedStringSegment. Note that \p Str must support
  /// reading one byte past the end.
  static StringRef getEncodedStringSegmentImpl(StringRef Str,
                                               SmallVectorImpl<char> &Buffer,
                                               bool IsFirstSegment,
                                               bool IsLastSegment,
                                               unsigned IndentToStrip,
                                               unsigned CustomDelimiterLen);

  /// \brief Compute the bytes that the actual string literal should codegen to.
  /// If a copy needs to be made, it will be allocated out of the provided
  /// \p Buffer.
  StringRef getEncodedStringSegment(StringSegment Segment,
                                    SmallVectorImpl<char> &Buffer) const {
    return getEncodedStringSegmentImpl(
        StringRef(getBufferPtrForSourceLoc(Segment.Loc), Segment.Length),
        Buffer, Segment.IsFirstSegment, Segment.IsLastSegment,
        Segment.IndentToStrip, Segment.CustomDelimiterLen);
  }

  /// \brief Given a string encoded with escapes like a string literal, compute
  /// the byte content.
  ///
  /// If a copy needs to be made, it will be allocated out of the provided
  /// \p Buffer.
  static StringRef getEncodedStringSegment(StringRef Str,
                                           SmallVectorImpl<char> &Buffer,
                                           bool IsFirstSegment = false,
                                           bool IsLastSegment = false,
                                           unsigned IndentToStrip = 0,
                                           unsigned CustomDelimiterLen = 0) {
    SmallString<128> TerminatedStrBuf(Str);
    TerminatedStrBuf.push_back('\0');
    StringRef TerminatedStr = StringRef(TerminatedStrBuf).drop_back();
    StringRef Result = getEncodedStringSegmentImpl(TerminatedStr, Buffer,
                                                   IsFirstSegment,
                                                   IsLastSegment,
                                                   IndentToStrip,
                                                   CustomDelimiterLen);
    if (Result == TerminatedStr)
      return Str;
    assert(Result.data() == Buffer.data());
    return Result;
  }

  /// \brief Given a string literal token, separate it into string/expr segments
  /// of a potentially interpolated string.
  static void getStringLiteralSegments(
      const Token &Str,
      SmallVectorImpl<StringSegment> &Segments,
      DiagnosticEngine *Diags);

  void getStringLiteralSegments(const Token &Str,
                                SmallVectorImpl<StringSegment> &Segments) {
    return getStringLiteralSegments(Str, Segments, Diags);
  }

  static SourceLoc getSourceLoc(const char *Loc) {
    return SourceLoc(llvm::SMLoc::getFromPointer(Loc));
  }

  /// Get the token that starts at the given location.
  Token getTokenAt(SourceLoc Loc);

  /// SILBodyRAII - This helper class is used when parsing a SIL body to inform
  /// the lexer that SIL-specific lexing should be enabled.
  struct SILBodyRAII {
    Lexer &L;
    SILBodyRAII(Lexer &L) : L(L) {
      assert(!L.InSILBody && "Already in a sil body?");
      L.InSILBody = true;
    }
    ~SILBodyRAII() {
      assert(L.InSILBody && "Left sil body already?");
      L.InSILBody = false;
    }
    SILBodyRAII(const SILBodyRAII&) = delete;
    void operator=(const SILBodyRAII&) = delete;
  };

private:
  /// Nul character meaning kind.
  enum class NulCharacterKind {
    /// String buffer terminator.
    BufferEnd,
    /// Embedded nul character.
    Embedded,
    /// Code completion marker.
    CodeCompletion
  };

  /// For a source location in the current buffer, returns the corresponding
  /// pointer.
  const char *getBufferPtrForSourceLoc(SourceLoc Loc) const {
    return BufferStart + SourceMgr.getLocOffsetInBuffer(Loc, BufferID);
  }

  void lexImpl();
  InFlightDiagnostic diagnose(const char *Loc, Diagnostic Diag);
  
  template<typename ...DiagArgTypes, typename ...ArgTypes>
  InFlightDiagnostic diagnose(const char *Loc, Diag<DiagArgTypes...> DiagID,
                              ArgTypes &&...Args) {
    return diagnose(Loc, Diagnostic(DiagID, std::forward<ArgTypes>(Args)...));
  }

  void formToken(tok Kind, const char *TokStart);
  void formEscapedIdentifierToken(const char *TokStart);
  void formStringLiteralToken(const char *TokStart, bool IsMultilineString,
                              unsigned CustomDelimiterLen);

  /// Advance to the end of the line.
  /// If EatNewLine is true, CurPtr will be at end of newline character.
  /// Otherwise, CurPtr will be at newline character.
  void skipToEndOfLine(bool EatNewline);

  /// Skip to the end of the line of a // comment.
  void skipSlashSlashComment(bool EatNewline);

  /// Skip a #! hashbang line.
  void skipHashbang(bool EatNewline);

  void skipSlashStarComment();
  void lexHash();
  void lexIdentifier();
  void lexDollarIdent();
  void lexOperatorIdentifier();
  void lexHexNumber();
  void lexNumber();
  void lexTrivia(syntax::Trivia &T, bool IsForTrailingTrivia);
  static unsigned lexUnicodeEscape(const char *&CurPtr, Lexer *Diags);

  unsigned lexCharacter(const char *&CurPtr, char StopQuote,
                        bool EmitDiagnostics, bool IsMultilineString = false,
                        unsigned CustomDelimiterLen = 0);
  void lexStringLiteral(unsigned CustomDelimiterLen = 0);
  void lexEscapedIdentifier();

  void tryLexEditorPlaceholder();
  const char *findEndOfCurlyQuoteStringLiteral(const char *,
                                               bool EmitDiagnostics);

  /// Try to lex conflict markers by checking for the presence of the start and
  /// end of the marker in diff3 or Perforce style respectively.
  bool tryLexConflictMarker(bool EatNewline);

  /// Returns it should be tokenize.
  bool lexUnknown(bool EmitDiagnosticsIfToken);

  NulCharacterKind getNulCharacterKind(const char *Ptr) const;
};
  
/// Given an ordered token \param Array , get the iterator pointing to the first
/// token that is not before \param Loc .
template<typename ArrayTy, typename Iterator = typename ArrayTy::iterator>
Iterator token_lower_bound(ArrayTy &Array, SourceLoc Loc) {
  return std::lower_bound(Array.begin(), Array.end(), Loc,
    [](const Token &T, SourceLoc L) {
      return T.getLoc().getOpaquePointerValue() < L.getOpaquePointerValue();
  });
}

/// Given an ordered token array \param AllTokens , get the slice of the array
/// where front() locates at \param StartLoc and back() locates at \param EndLoc .
ArrayRef<Token> slice_token_array(ArrayRef<Token> AllTokens, SourceLoc StartLoc,
                                  SourceLoc EndLoc);

} // end namespace swift

#endif
