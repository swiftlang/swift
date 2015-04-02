//===--- Lexer.h - Swift Language Lexer -------------------------*- C++ -*-===//
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
//  This file defines the Lexer interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LEXER_H
#define SWIFT_LEXER_H

#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Token.h"
#include "swift/AST/DiagnosticEngine.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
  class DiagnosticEngine;
  class InFlightDiagnostic;
  class LangOptions;

  template<typename ...T> struct Diag;

enum class CommentRetentionMode {
  None,
  AttachToNextToken,
  ReturnAsTokens,
};

class Lexer {
  const LangOptions &LangOpts;
  const SourceManager &SourceMgr;
  DiagnosticEngine *Diags;
  const unsigned BufferID;

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

  /// Pointer to the next not consumed character.
  const char *CurPtr;

  /// @{
  /// Members that are *not* permanent lexer state.  The values only make sense
  /// during the lexImpl() invocation.  These variables are declared as members
  /// rather than locals so that we don't have to thread them through to all
  /// lexing helpers.

  /// Points to the point in the source buffer where we started scanning for
  /// the current token.  Thus, the range [LastCommentBlockStart, CurPtr)
  /// covers all comments and whitespace that we skipped, and the token itself.
  const char *LastCommentBlockStart = nullptr;

  /// True if we have seen a comment while scanning for the current token.
  bool SeenComment = false;

  /// @}

  Token NextToken;
  
  /// \brief This is true if we're lexing a .sil file instead of a .swift
  /// file.  This enables the 'sil' keyword.
  const bool InSILMode;

  const CommentRetentionMode RetainComments;

  /// InSILBody - This is true when we're lexing the body of a SIL declaration
  /// in a SIL file.  This enables some context-sensitive lexing.
  bool InSILBody = false;
  
public:
  /// \brief Lexer state can be saved/restored to/from objects of this class.
  class State {
  public:
    State() {}

    bool isValid() const {
      return Loc.isValid();
    }

    State advance(unsigned Offset) const {
      assert(isValid());
      return State(Loc.getAdvancedLoc(Offset));
    }

  private:
    explicit State(SourceLoc Loc): Loc(Loc) {}
    SourceLoc Loc;
    friend class Lexer;
  };

private:
  Lexer(const Lexer&) = delete;
  void operator=(const Lexer&) = delete;

  /// The principal constructor used by public constructors below.
  /// Don't use this constructor for other purposes, it does not initialize
  /// everything.
  Lexer(const LangOptions &Options,
        const SourceManager &SourceMgr, DiagnosticEngine *Diags,
        unsigned BufferID, bool InSILMode,
        CommentRetentionMode RetainComments);

  /// @{
  /// Helper routines used in \c Lexer constructors.
  void primeLexer();
  void initSubLexer(Lexer &Parent, State BeginState, State EndState);
  /// @}

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
  ///   means that APIs like getLofForEndOfToken really ought to take
  ///   this flag; it's just that we don't care that much about fidelity
  ///   when parsing SIL files.
  Lexer(const LangOptions &Options,
        const SourceManager &SourceMgr, unsigned BufferID,
        DiagnosticEngine *Diags, bool InSILMode,
        CommentRetentionMode RetainComments = CommentRetentionMode::None)
      : Lexer(Options, SourceMgr, Diags, BufferID, InSILMode, RetainComments) {
    primeLexer();
  }

  /// \brief Create a lexer that scans a subrange of the source buffer.
  Lexer(const LangOptions &Options,
        const SourceManager &SourceMgr, unsigned BufferID,
        DiagnosticEngine *Diags, bool InSILMode,
        CommentRetentionMode RetainComments,
        unsigned Offset, unsigned EndOffset)
      : Lexer(Options, SourceMgr, Diags, BufferID, InSILMode, RetainComments) {
    assert(Offset <= EndOffset && "invalid range");
    initSubLexer(
        *this,
        State(getLocForStartOfBuffer().getAdvancedLoc(Offset)),
        State(getLocForStartOfBuffer().getAdvancedLoc(EndOffset)));
  }

  /// \brief Create a sub-lexer that lexes from the same buffer, but scans
  /// a subrange of the buffer.
  ///
  /// \param Parent the parent lexer that scans the whole buffer
  /// \param BeginState start of the subrange
  /// \param EndState end of the subrange
  Lexer(Lexer &Parent, State BeginState, State EndState)
      : Lexer(Parent.LangOpts, Parent.SourceMgr, Parent.Diags, Parent.BufferID,
              Parent.InSILMode, Parent.RetainComments) {
    initSubLexer(Parent, BeginState, EndState);
  }

  /// \brief Returns true if this lexer will produce a code completion token.
  bool isCodeCompletion() const {
    return CodeCompletionPtr != nullptr;
  }

  void lex(Token &Result) {
    Result = NextToken;
    if (Result.isNot(tok::eof))
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
  State getStateForBeginningOfToken(const Token &Tok) const {
    // If the token has a comment attached to it, rewind to before the comment,
    // not just the start of the token.  This ensures that we will re-lex and
    // reattach the comment to the token if rewound to this state.
    SourceLoc TokStart = Tok.getCommentStart();
    if (TokStart.isInvalid())
      TokStart = Tok.getLoc();
    return getStateForBeginningOfTokenLoc(TokStart);
  }

  State getStateForEndOfTokenLoc(SourceLoc Loc) const {
    return State(getLocForEndOfToken(SourceMgr, Loc));
  }

  /// \brief Restore the lexer state to a given one, that can be located either
  /// before or after the current position.
  void restoreState(State S) {
    assert(S.isValid());
    CurPtr = getBufferPtrForSourceLoc(S.Loc);
    // Don't reemit diagnostics while readvancing the lexer.
    auto TmpDiags = Diags;
    Diags = nullptr;
    lexImpl();
    Diags = TmpDiags;
  }

  /// \brief Restore the lexer state to a given state that is located before
  /// current position.
  void backtrackToState(State S) {
    assert(getBufferPtrForSourceLoc(S.Loc) <= CurPtr &&
           "can't backtrack forward");
    restoreState(S);
  }

  /// \brief Retrieve the source location that points just past the
  /// end of the token refered to by \c Loc.
  ///
  /// \param SM The source manager in which the given source location
  /// resides.
  ///
  /// \param Loc The source location of the beginning of a token.
  static SourceLoc getLocForEndOfToken(const SourceManager &SM, SourceLoc Loc);

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

  /// Retrieve the start location of the line containing the given location.
  /// the given location.
  static SourceLoc getLocForStartOfLine(SourceManager &SM, SourceLoc Loc);

  /// Retrieve the source location for the end of the line containing the
  /// given token, which is the location of the start of the next line.
  static SourceLoc getLocForEndOfLine(SourceManager &SM, SourceLoc Loc);

  /// Retrieve the string used to indent the line that contains the given
  /// source location.
  static StringRef getIndentationForLine(SourceManager &SM, SourceLoc Loc);

  /// \brief Determines if the given string is a valid non-operator
  /// identifier, without escaping characters.
  static bool isIdentifier(StringRef identifier);

  /// \brief Determine the token kind of the string, given that it is a valid
  /// non-operator identifier. Return tok::identifier if the string is not a
  /// reserved word.
  static tok kindOfIdentifier(StringRef Str, bool InSILMode);

  SourceLoc getLocForStartOfBuffer() const {
    return SourceLoc(llvm::SMLoc::getFromPointer(BufferStart));
  }
  
  /// StringSegment - A segment of a (potentially interpolated) string.
  struct StringSegment {
    enum : char { Literal, Expr } Kind;
    // Loc+Length for the segment inside the string literal, without quotes.
    SourceLoc Loc;
    unsigned Length;
    
    static StringSegment getLiteral(SourceLoc Loc, unsigned Length) {
      StringSegment Result;
      Result.Kind = Literal;
      Result.Loc = Loc;
      Result.Length = Length;
      return Result;
    }
    
    static StringSegment getExpr(SourceLoc Loc, unsigned Length) {
      StringSegment Result;
      Result.Kind = Expr;
      Result.Loc = Loc;
      Result.Length = Length;
      return Result;
    }
  };
  
  /// \brief Compute the bytes that the actual string literal should codegen to.
  /// If a copy needs to be made, it will be allocated out of the provided
  /// Buffer.
  static StringRef getEncodedStringSegment(StringRef Str,
                                           SmallVectorImpl<char> &Buffer);
  StringRef getEncodedStringSegment(StringSegment Segment,
                                    SmallVectorImpl<char> &Buffer) const {
    return getEncodedStringSegment(
        StringRef(getBufferPtrForSourceLoc(Segment.Loc), Segment.Length),
        Buffer);
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

  /// \brief Return the UTF32 codepoint for the specified character literal.
  uint32_t getEncodedCharacterLiteral(const Token &Tok);

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
  /// For a source location in the current buffer, returns the corresponding
  /// pointer.
  const char *getBufferPtrForSourceLoc(SourceLoc Loc) const {
    return BufferStart + SourceMgr.getLocOffsetInBuffer(Loc, BufferID);
  }

  StringRef getSubstring(const char *Start, unsigned Length) const {
    assert(Start >= BufferStart && Start <= BufferEnd);
    unsigned BytesUntilBufferEnd = BufferEnd - Start;
    if (Length > BytesUntilBufferEnd)
      Length = BytesUntilBufferEnd;
    return StringRef(Start, Length);
  }

  void lexImpl();
  InFlightDiagnostic diagnose(const char *Loc, Diagnostic Diag);
  
  template<typename ...DiagArgTypes, typename ...ArgTypes>
  InFlightDiagnostic diagnose(const char *Loc, Diag<DiagArgTypes...> DiagID,
                              ArgTypes &&...Args) {
    return diagnose(Loc, Diagnostic(DiagID, std::forward<ArgTypes>(Args)...));
  }

  void formToken(tok Kind, const char *TokStart);

  void skipToEndOfLine();

  /// Skip to the end of the line of a // comment.
  void skipSlashSlashComment();

  /// Skip a #! hashbang line.
  void skipHashbang();

  void skipSlashStarComment();
  void lexIdentifier();
  void lexDollarIdent();
  void lexOperatorIdentifier();
  void lexHexNumber();
  void lexNumber();
  static unsigned lexUnicodeEscape(const char *&CurPtr, Lexer *Diags);

  unsigned lexCharacter(const char *&CurPtr,
                        bool StopAtDoubleQuote, bool EmitDiagnostics);
  void lexCharacterLiteral();
  void lexStringLiteral();
  void lexEscapedIdentifier();

  void tryLexEditorPlaceholder();
};
  
  
} // end namespace swift

#endif
