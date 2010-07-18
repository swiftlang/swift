//===--- Parser.h - Swift Language Parser -----------------------*- C++ -*-===//
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
//  This file defines the Parser interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSER_H
#define SWIFT_PARSER_H

namespace llvm {
  class SourceMgr;
}

namespace swift {
  class Lexer;
  
class Parser {
  llvm::SourceMgr &SourceMgr;
  Lexer *L;
  
  Parser(const Parser&);         // DO NOT IMPLEMENT
  void operator=(const Parser&); // DO NOT IMPLEMENT
public:
  Parser(unsigned BufferID, llvm::SourceMgr &SM);
  ~Parser();
  
  void ParseTranslationUnit();
  
  
private:
  void Warning(const char *Loc, const char *Message);
  void Error(const char *Loc, const char *Message);
};
  
} // end namespace swift

#endif
