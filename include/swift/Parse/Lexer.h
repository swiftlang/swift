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

#include "Token.h"
#include "llvm/ADT/SmallVector.h"
#include "swift/Basic/SourceLoc.h"

namespace llvm {
  class SourceMgr;
}

namespace swift {
  class DiagnosticEngine;
  class Identifier;
  class InFlightDiagnostic;
  class ASTContext;
  
  template<typename ...T> struct Diag;

class Lexer {
  llvm::SourceMgr &SourceMgr;
  DiagnosticEngine *Diags;

  /// Pointer to the first character of the buffer.
  const char *BufferStart;

  /// Pointer to one past the end character of the buffer.  Because the buffer
  /// is always NUL-terminated, this points to the NUL terminator.
  const char *BufferEnd;

  /// Pointer to the artificial EOF that is located before BufferEnd.  Useful
  /// for lexing subranges of a buffer.
  const char *ArtificialEOF = nullptr;

  /// If non-null, points to the '\0' character in the buffer where we should
  /// produce a code completion token.
  const char *CodeCompletionPtr = nullptr;

  /// Pointer to the next not consumed character.
  const char *CurPtr;

  Token NextToken;
  
  /// InSILMode - This is true if we're lexing a .sil file instead of a .swift
  /// file.  This enables the 'sil' keyword.
  bool InSILMode;

  /// InSILBody - This is true when we're lexing the body of a SIL declaration
  /// in a SIL file.  This enables some context-sensitive lexing.
  bool InSILBody = false;
  
  /// \brief Set to true to return comment tokens, instead of skipping them.
  bool KeepComments = false;

  /// \brief If true, we allow and skip the #! hashbang line at the beginning
  /// of the buffer.
  bool AllowHashbang = false;

  Lexer(const Lexer&) = delete;
  void operator=(const Lexer&) = delete;

  Lexer(llvm::SourceMgr &SourceMgr, llvm::StringRef Buffer,
        DiagnosticEngine *Diags, const char *CurrentPosition,
        bool InSILMode, bool KeepComments, bool AllowHashbang, bool Prime);

  void primeLexer();

public:
  Lexer(llvm::StringRef Buffer, llvm::SourceMgr &SourceMgr,
        DiagnosticEngine *Diags, bool InSILMode, bool KeepComments = false,
        bool AllowHashbang = false)
      : Lexer(SourceMgr, Buffer, Diags, Buffer.begin(), InSILMode,
              KeepComments, AllowHashbang, /*Prime=*/true) {
  }

  /// \brief Lexer state can be saved/restored to/from objects of this class.
  class State {
  public:
    State(): CurPtr(nullptr) {}

  private:
    explicit State(const char *CurPtr): CurPtr(CurPtr) {}
    const char *CurPtr;
    bool isValid() const {
      return CurPtr != nullptr;
    }
    friend class Lexer;
  };

  /// \brief Create a sub-lexer that lexes from the same buffer, but scans
  /// a subrange of the buffer.
  ///
  /// \param Parent the parent lexer that scans the whole buffer
  /// \param BeginState start of the subrange
  /// \param EndState end of the subrange
  Lexer(Lexer &Parent, State BeginState, State EndState,
        llvm::SourceMgr &SourceMgr, DiagnosticEngine *Diags, bool InSILMode)
    : Lexer(SourceMgr,
            StringRef(BeginState.CurPtr, Parent.BufferEnd - BeginState.CurPtr),
            Diags, BeginState.CurPtr, InSILMode, Parent.isKeepingComments(),
            Parent.AllowHashbang, /*Prime=*/false) {
    assert(BeginState.CurPtr >= Parent.BufferStart &&
           BeginState.CurPtr <= Parent.BufferEnd &&
           "Begin position out of range");
    // If the parent lexer should stop prematurely, and the ArtificialEOF
    // position is in this subrange, then we should stop at that point, too.
    if (Parent.ArtificialEOF &&
        Parent.ArtificialEOF >= BufferStart &&
        Parent.ArtificialEOF <= BufferEnd) {
      ArtificialEOF = Parent.ArtificialEOF;
    } else
      ArtificialEOF = EndState.CurPtr;

    // Do code completion if the parent lexer is doing code completion and the
    // code completion token is in subrange.
    if (Parent.CodeCompletionPtr &&
        Parent.CodeCompletionPtr >= BufferStart &&
        Parent.CodeCompletionPtr <= BufferEnd)
      CodeCompletionPtr = Parent.CodeCompletionPtr;

    primeLexer();
  }

  bool isKeepingComments() const { return KeepComments; }

  /// \brief Returns true if this lexer will produce a code completion token.
  bool isCodeCompletion() const {
    return CodeCompletionPtr != nullptr;
  }

  void setCodeCompletion(unsigned Offset) {
    assert(CodeCompletionPtr == nullptr ||
           CodeCompletionPtr == BufferStart + Offset);
    CodeCompletionPtr = BufferStart + Offset;
    assert(CodeCompletionPtr <= BufferEnd);
  }

  const char *getBufferEnd() const { return BufferEnd; }

  void lex(Token &Result) {
    Result = NextToken;
    if (Result.isNot(tok::eof))
      lexImpl();
  }

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
    return getStateForBeginningOfTokenLoc(Tok.getLoc());
  }

  State getStateForEndOfTokenLoc(SourceLoc Loc) const {
    return State(getLocForEndOfToken(SourceMgr, Loc).Value.getPointer());
  }

  /// \brief Restore the lexer state to a given one, that can be located either
  /// before or after the current position.
  void restoreState(State S) {
    assert(S.isValid());
    assert(BufferStart <= S.CurPtr && S.CurPtr <= BufferEnd &&
           "state for the wrong buffer");
    CurPtr = S.CurPtr;
    lexImpl();
  }

  /// \brief Restore the lexer state to a given state that is located before
  /// current position.
  void backtrackToState(State S) {
    assert(S.CurPtr <= CurPtr && "can't backtrack forward");
    restoreState(S);
  }

  /// \brief Retrieve the source location that points just past the
  /// end of the token refered to by \c Loc.
  ///
  /// \param SM The source manager in which the given source location
  /// resides.
  ///
  /// \param Loc The source location of the beginning of a token.
  static SourceLoc getLocForEndOfToken(llvm::SourceMgr &SM, SourceLoc Loc);

  /// \brief Determines if the given string is a valid non-operator
  /// identifier.
  static bool isIdentifier(llvm::StringRef identifier);

  SourceLoc getLocForStartOfBuffer() const {
    return SourceLoc(llvm::SMLoc::getFromPointer(BufferStart));
  }
  
  /// StringSegment - A segment of a (potentially interpolated) string.
  struct StringSegment {
    enum : char { Literal, Expr } Kind;
    /// String data (not quoted).  It might not point into the original source
    /// buffer.
    StringRef Data;
    SourceRange Range;
    
    static StringSegment getLiteral(StringRef Str, SourceRange Range) {
      StringSegment Result;
      Result.Kind = Literal;
      Result.Data = Str;
      Result.Range = Range;
      return Result;
    }
    
    static StringSegment getExpr(StringRef Str, SourceRange Range) {
      StringSegment Result;
      Result.Kind = Expr;
      Result.Data = Str;
      Result.Range = Range;
      return Result;
    }
  };
  
  /// getEncodedStringLiteral - Given a string literal token, compute the bytes
  /// that the actual string literal should codegen to along with any
  /// sequences that represent interpolated expressions.
  /// If a copy needs to be made, it will be allocated out of the ASTContext
  /// allocator.
  void getEncodedStringLiteral(const Token &Str, ASTContext &Ctx,
                               llvm::SmallVectorImpl<StringSegment> &Segments);
  /// getEncodedCharacterLiteral - Return the UTF32 codepoint for the specified
  /// character literal.
  uint32_t getEncodedCharacterLiteral(const Token &Str); 

  InFlightDiagnostic diagnose(const char *Loc, Diag<> ID);

  static SourceLoc getSourceLoc(const char *Loc) {
    return SourceLoc(llvm::SMLoc::getFromPointer(Loc));
  }

  /// getTokenKind - Retrieve the token kind for the given text, which must
  /// fall within the given source buffer.
  tok getTokenKind(StringRef Text);
  
  void lexHexNumber();

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
  void lexImpl();
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
  void lexNumber();
  
  unsigned lexCharacter(const char *&CurPtr,
                        bool StopAtDoubleQuote, bool EmitDiagnostics);
  void lexCharacterLiteral();
  void lexStringLiteral();
};
  
  
} // end namespace swift

#endif
