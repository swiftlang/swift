//===--- ParserTokenRange.h - Saving/restoring parser state -----*- C++ -*-===//
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

#ifndef SWIFT_AST_TOKEN_RANGE_H
#define SWIFT_AST_TOKEN_RANGE_H

/// \brief An opaque object that stores the parser state for delayed parsing.
class ParserTokenRange {
  // These pointers are meaningful for the parser.  Memory is allocated on the
  // permanent arena of the ASTContext.
  void *BeginParserState;
  void *EndLexerState;

public:
  ParserTokenRange():
      BeginParserState(nullptr),
      EndLexerState(nullptr)
  {}

  ParserTokenRange(void *BeginParserState, void *EndLexerState):
      BeginParserState(BeginParserState),
      EndLexerState(EndLexerState)
  {}

  template<typename T>
  T *getBeginParserState() {
    return static_cast<T *>(BeginParserState);
  }

  template<typename T>
  T *getEndLexerState() {
    return static_cast<T *>(EndLexerState);
  }
};

#endif // LLVM_SWIFT_AST_TOKEN_RANGE_H

