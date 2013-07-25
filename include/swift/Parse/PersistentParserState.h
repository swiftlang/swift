//===--- PersistentParserState.h - Parser State -----------------*- C++ -*-===//
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
// Parser state persistent across multiple parses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSE_PERSISTENTPARSERSTATE_H
#define SWIFT_PARSE_PERSISTENTPARSERSTATE_H

#include "swift/Basic/SourceLoc.h"
#include "swift/Parse/Scope.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
  class FuncExpr;

/// \brief Parser state persistent across multiple parses.
class PersistentParserState {

  class FunctionBodyState {
    SourceRange BodyRange;
    SourceLoc PreviousLoc;
    SavedScope Scope;
    friend class Parser;

    SavedScope takeScope() {
      return std::move(Scope);
    }

  public:
    FunctionBodyState(SourceRange BodyRange, SourceLoc PreviousLoc,
                      SavedScope &&Scope)
      : BodyRange(BodyRange), PreviousLoc(PreviousLoc), Scope(std::move(Scope))
    {}
  };

  ScopeInfo ScopeInfo;
  typedef llvm::DenseMap<FuncExpr *, std::unique_ptr<FunctionBodyState>>
      DelayedBodiesTy;
  DelayedBodiesTy DelayedBodies;
  SourceLoc ParserPos;
  SourceLoc PrevParserLoc;

public:
  swift::ScopeInfo &getScopeInfo() { return ScopeInfo; }

  void delayFunctionBodyParsing(FuncExpr *FE, SourceRange BodyRange,
                                SourceLoc PreviousLoc);
  std::unique_ptr<FunctionBodyState> takeBodyState(FuncExpr *FE);

  void markParserPosition(SourceLoc Loc, SourceLoc PrevLoc) {
    ParserPos = Loc;
    PrevParserLoc = PrevLoc;
  }

  std::pair<SourceLoc, SourceLoc> takeParserPosition() {
    auto Pos = std::make_pair(ParserPos, PrevParserLoc);
    ParserPos = SourceLoc();
    PrevParserLoc = SourceLoc();
    return Pos;
  }
};

} // end namespace swift

#endif
