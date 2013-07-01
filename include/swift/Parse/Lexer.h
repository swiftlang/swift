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
  const char *ArtificialEOF;

  /// Pointer to the next not consumed character.
  const char *CurPtr;

  Token NextToken;
  
  /// InSILMode - This is true if we're lexing a .sil file instead of a .swift
  /// file.  This enables the 'sil' keyword.
  bool InSILMode;

  /// InSILBody - This is true when we're lexing the body of a SIL declaration
  /// in a SIL file.  This enables some context-sensitive lexing.
  bool InSILBody = false;
  
  Lexer(const Lexer&) = delete;
  void operator=(const Lexer&) = delete;

  Lexer(llvm::StringRef Buffer, llvm::SourceMgr &SourceMgr,
        DiagnosticEngine *Diags, const char *CurrentPosition, bool InSILMode);

public:
  Lexer(llvm::StringRef Buffer, llvm::SourceMgr &SourceMgr,
        DiagnosticEngine *Diags, bool InSILMode)
    : Lexer(Buffer, SourceMgr, Diags, Buffer.begin(), InSILMode) { }

  /// \brief Lexer state can be saved/restored to/from objects of this class.
  class State {
    State(const char *CurPtr): CurPtr(CurPtr) {}
    const char *CurPtr;
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
    : Lexer(StringRef(BeginState.CurPtr, Parent.BufferEnd - BeginState.CurPtr),
            SourceMgr, Diags, BeginState.CurPtr, InSILMode) {
    assert(BeginState.CurPtr >= Parent.BufferStart &&
           BeginState.CurPtr <= Parent.BufferEnd &&
           "Begin position out of range");
    ArtificialEOF = EndState.CurPtr;
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

  /// \brief Returns the lexer state for the beginning of the given token.
  /// After restoring the state, lexer will return this token and continue from
  /// there.
  State getStateForBeginningOfToken(const Token &Tok) const {
    const char *Ptr = Tok.getText().begin();
    // Skip whitespace backwards until we hit a newline.  This is needed to
    // correctly lex the token if it is at the beginning of the line.
    while (Ptr >= BufferStart + 1) {
      char C = Ptr[-1];
      if (C == ' ' || C == '\t' || C == 0) {
        Ptr--;
        continue;
      }
      if (C == '\n' || C == '\r') {
        Ptr--;
        break;
      }
      break;
    }
    return State(Ptr);
  }

  /// \brief Restore the lexer state to a given one, that can be located either
  /// before or after the current position.
  void restoreState(State S) {
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
    StringRef Data;
    
    static StringSegment getLiteral(StringRef Str) {
      StringSegment Result;
      Result.Kind = Literal;
      Result.Data = Str;
      return Result;
    }
    
    static StringSegment getExpr(StringRef Str) {
      StringSegment Result;
      Result.Kind = Expr;
      Result.Data = Str;
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

  void skipSlashSlashComment();
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
