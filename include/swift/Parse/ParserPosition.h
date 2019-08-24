//===--- ParserPosition.h - Parser Position ---------------------*- C++ -*-===//
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
// Parser position where Parser can jump to.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSE_PARSERPOSITION_H
#define SWIFT_PARSE_PARSERPOSITION_H

#include "swift/Basic/SourceLoc.h"
#include "swift/Parse/LexerState.h"

namespace swift {

class ParserPosition {
  LexerState LS;
  SourceLoc PreviousLoc;
  friend class Parser;

  ParserPosition(LexerState LS, SourceLoc PreviousLoc)
      : LS(LS), PreviousLoc(PreviousLoc) {}
public:
  ParserPosition() = default;
  ParserPosition &operator=(const ParserPosition &) = default;

  bool isValid() const { return LS.isValid(); }
};

} // end namespace swift

#endif
