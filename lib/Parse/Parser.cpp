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
#include "swift/Parse/Lexer.h"
using namespace swift;

Parser::Parser(unsigned BufferID, llvm::SourceMgr &SM) : SourceMgr(SM) {
  L = new Lexer(BufferID, SM);
}

Parser::~Parser() {
  delete L; 
}

void Parser::ParseTranslationUnit() {
  
}
