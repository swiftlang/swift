//===--- Read.cpp - SIL File Parsing logic --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILParser.h"

using namespace swift;
using namespace swift::syntax;

//===----------------------------------------------------------------------===//
// ReadSIL implementation
//===----------------------------------------------------------------------===//

namespace swift {

bool ReadSIL::readSingleID(SILParserValues &instResults) {
  if (!Tok.getRawText().equals("%")) {
    return false;
  }
  consumeToken();
  if (Tok.is(tok::identifier)) {
    instResults.push_back(Tok.getRawText());
    return true;
  }
  return false;
}

SILParserResult ReadSIL::read() {
  ParserPosition start = getParserPosition();
  SILParserResult readingResult;
  readingResult.loc.begin = Tok.getLoc();
  readingResult.loc.end = Tok.getLoc();

  // Read the results, if any:
  // (%x,)? | %x
  SILParserValues instResults;
  if (consumeIf(tok::l_paren)) {
    while (!consumeIf(tok::r_paren)) {
      readingResult.loc.end = Tok.getLoc();

      if (!readSingleID(instResults)) {
        return {};
      }

      if (Tok.is(tok::comma)) {
        consumeToken();
      } else if (!Tok.is(tok::r_paren)) {
        return {};
      }
    }
  } else if (Tok.getRawText().equals("%")) {
    if (!readSingleID(instResults)) {
      return {};
    }
  }

  // Read the SILInstruction Kind.
  if (!Tok.is(tok::identifier)) {
    return {};
  }

  auto instStr = Tok.getRawText();
  consumeToken();
  readingResult.loc.end = Tok.getLoc();

  if (false) {
  }
#define FULL_INST(ID, NAME, PARENT, MEMBEHAVIOR, MAYRELEASE)                   \
  else if (instStr == #NAME) {                                                 \
    readingResult.kind = SILInstructionKind::ID;                               \
    read##ID(readingResult);                                                   \
  }
#include "swift/SIL/SILNodes.def"
  else
    llvm_unreachable("Unhandled SILNode");
  readingResult.results = instResults;
  
  // If we couldn't read the instruction, reset the parser position for the old
  // parser (which we will bail out to).
  if (unsigned(readingResult.kind) == 0) {
    backtrackToPosition(start);
  }
  
  return readingResult;
}

//===----------------------------------------------------------------------===//
// ReadSIL visitors
//===----------------------------------------------------------------------===//

} // namespace swift
